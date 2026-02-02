// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.json.coder;

/**
 * JSON encoding and decoding for Hydra terms
 */
public interface Coder {
  static <T0> hydra.compute.Flow<hydra.graph.Graph, hydra.compute.Coder<hydra.graph.Graph, T0, hydra.core.Term, hydra.json.model.Value>> jsonCoder(hydra.core.Type typ) {
    return hydra.lib.flows.Bind.apply(
      hydra.adapt.modules.Modules.languageAdapter(
        hydra.ext.org.json.language.Language.jsonLanguage(),
        (typ)),
      (java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T0, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.compute.Coder<hydra.graph.Graph, T0, hydra.core.Term, hydra.json.model.Value>>>) (adapter -> hydra.lib.flows.Bind.apply(
        hydra.ext.org.json.coder.Coder.termCoder(((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T0, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T0, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T0, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T0, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T0, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T0, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))))).apply((adapter))),
        (java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T0, hydra.core.Term, hydra.json.model.Value>, hydra.compute.Flow<hydra.graph.Graph, hydra.compute.Coder<hydra.graph.Graph, T0, hydra.core.Term, hydra.json.model.Value>>>) (coder -> hydra.lib.flows.Pure.apply(hydra.adapt.utils.Utils.composeCoders(
          ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T0, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.graph.Graph, T0, hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T0, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.graph.Graph, T0, hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T0, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.graph.Graph, T0, hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T0, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.graph.Graph, T0, hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T0, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.graph.Graph, T0, hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.graph.Graph, T0, hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.graph.Graph, T0, hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))))).apply((adapter)),
          (coder)))))));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T0, hydra.compute.Coder<T1, T2, hydra.core.Literal, hydra.json.model.Value>> literalJsonCoder(hydra.core.LiteralType lt) {
    return hydra.lib.flows.Pure.apply(hydra.ext.org.json.coder.Coder.<T1, T2>literalJsonCoder_encoded((lt)));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Literal> literalJsonCoder_decodeBool(hydra.json.model.Value s) {
    return ((s)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Literal> otherwise(hydra.json.model.Value instance) {
        return hydra.monads.Monads.unexpected(
          "boolean",
          hydra.ext.org.json.coder.Coder.showValue((s)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Literal> visit(hydra.json.model.Value.Boolean_ b) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Literal.Boolean_(((b)).value));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Literal> literalJsonCoder_decodeFloat(hydra.json.model.Value s) {
    return ((s)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Literal> otherwise(hydra.json.model.Value instance) {
        return hydra.monads.Monads.unexpected(
          "number",
          hydra.ext.org.json.coder.Coder.showValue((s)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Literal> visit(hydra.json.model.Value.Number_ f) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Bigfloat(((f)).value)));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Literal> literalJsonCoder_decodeInteger(hydra.json.model.Value s) {
    return ((s)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Literal> otherwise(hydra.json.model.Value instance) {
        return hydra.monads.Monads.unexpected(
          "number",
          hydra.ext.org.json.coder.Coder.showValue((s)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Literal> visit(hydra.json.model.Value.Number_ f) {
        java.math.BigInteger bi = hydra.lib.literals.BigfloatToBigint.apply(((f)).value);
        return hydra.lib.flows.Pure.apply(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Bigint((bi))));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Literal> literalJsonCoder_decodeString(hydra.json.model.Value s) {
    return ((s)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Literal> otherwise(hydra.json.model.Value instance) {
        return hydra.monads.Monads.unexpected(
          "string",
          hydra.ext.org.json.coder.Coder.showValue((s)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Literal> visit(hydra.json.model.Value.String_ s_) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Literal.String_(((s_)).value));
      }
    });
  }
  
  static <T0, T1> hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value> literalJsonCoder_encoded(hydra.core.LiteralType lt) {
    return ((lt)).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value> visit(hydra.core.LiteralType.Boolean_ ignored) {
        return (hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>) ((hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>) ((hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>) ((hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>) (new hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>((java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<T0, hydra.json.model.Value>>) (lit -> hydra.lib.flows.Bind.apply(
          hydra.extract.core.Core.<T0>booleanLiteral((lit)),
          (java.util.function.Function<Boolean, hydra.compute.Flow<T0, hydra.json.model.Value>>) (b -> hydra.lib.flows.Pure.apply(new hydra.json.model.Value.Boolean_((b)))))), p0 -> hydra.ext.org.json.coder.Coder.<T1>literalJsonCoder_decodeBool((p0)))))));
      }
      
      @Override
      public hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value> visit(hydra.core.LiteralType.Float_ ignored) {
        return (hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>) ((hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>) ((hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>) ((hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>) (new hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>((java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<T0, hydra.json.model.Value>>) (lit -> hydra.lib.flows.Bind.apply(
          hydra.extract.core.Core.<T0>floatLiteral((lit)),
          (java.util.function.Function<hydra.core.FloatValue, hydra.compute.Flow<T0, hydra.json.model.Value>>) (f -> hydra.lib.flows.Bind.apply(
            hydra.extract.core.Core.<T0>bigfloatValue((f)),
            (java.util.function.Function<java.math.BigDecimal, hydra.compute.Flow<T0, hydra.json.model.Value>>) (bf -> hydra.lib.flows.Pure.apply(new hydra.json.model.Value.Number_((bf)))))))), p0 -> hydra.ext.org.json.coder.Coder.<T1>literalJsonCoder_decodeFloat((p0)))))));
      }
      
      @Override
      public hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value> visit(hydra.core.LiteralType.Integer_ ignored) {
        return (hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>) ((hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>) ((hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>) ((hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>) (new hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>((java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<T0, hydra.json.model.Value>>) (lit -> hydra.lib.flows.Bind.apply(
          hydra.extract.core.Core.<T0>integerLiteral((lit)),
          (java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<T0, hydra.json.model.Value>>) (i -> hydra.lib.flows.Bind.apply(
            hydra.extract.core.Core.<T0>bigintValue((i)),
            (java.util.function.Function<java.math.BigInteger, hydra.compute.Flow<T0, hydra.json.model.Value>>) (bi -> hydra.lib.flows.Pure.apply(new hydra.json.model.Value.Number_(hydra.lib.literals.BigintToBigfloat.apply((bi))))))))), p0 -> hydra.ext.org.json.coder.Coder.<T1>literalJsonCoder_decodeInteger((p0)))))));
      }
      
      @Override
      public hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value> visit(hydra.core.LiteralType.String_ ignored) {
        return (hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>) ((hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>) ((hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>) ((hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>) (new hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.json.model.Value>((java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<T0, hydra.json.model.Value>>) (lit -> hydra.lib.flows.Bind.apply(
          hydra.extract.core.Core.<T0>stringLiteral((lit)),
          (java.util.function.Function<String, hydra.compute.Flow<T0, hydra.json.model.Value>>) (s -> hydra.lib.flows.Pure.apply(new hydra.json.model.Value.String_((s)))))), p0 -> hydra.ext.org.json.coder.Coder.<T1>literalJsonCoder_decodeString((p0)))))));
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<T0, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>> recordCoder(hydra.core.RowType rt) {
    java.util.List<hydra.core.FieldType> fields = ((rt)).fields;
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.MapList.apply(
        p0 -> hydra.ext.org.json.coder.Coder.<T0, T1>recordCoder_getCoder((p0)),
        (fields)),
      (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.FieldType, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>>>, hydra.compute.Flow<T0, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>>>) (coders -> hydra.lib.flows.Pure.apply((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) (new hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.json.model.Value>>) (v1 -> hydra.ext.org.json.coder.Coder.<T1>encodeRecord(
        (coders),
        (v1))), (java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Term>>) (v1 -> hydra.ext.org.json.coder.Coder.decodeRecord(
        (rt),
        (coders),
        (v1)))))))))));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<hydra.core.FieldType, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>>> recordCoder_getCoder(hydra.core.FieldType f) {
    return hydra.lib.flows.Bind.apply(
      hydra.ext.org.json.coder.Coder.<T0, T1>termCoder(((f)).type),
      (java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<hydra.core.FieldType, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>>>>) (coder -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<hydra.core.FieldType, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>>) ((hydra.util.Tuple.Tuple2<hydra.core.FieldType, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>>) (new hydra.util.Tuple.Tuple2<hydra.core.FieldType, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>>((f), (coder)))))));
  }
  
  static <T0> hydra.compute.Flow<hydra.graph.Graph, hydra.json.model.Value> encodeRecord(java.util.List<hydra.util.Tuple.Tuple2<hydra.core.FieldType, hydra.compute.Coder<hydra.graph.Graph, T0, hydra.core.Term, hydra.json.model.Value>>> coders, hydra.core.Term term) {
    hydra.core.Term stripped = hydra.rewriting.Rewriting.deannotateTerm((term));
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.termRecord((stripped)),
      (java.util.function.Function<hydra.core.Record, hydra.compute.Flow<hydra.graph.Graph, hydra.json.model.Value>>) (record -> {
        java.util.List<hydra.core.Field> fields = ((record)).fields;
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            p0 -> hydra.ext.org.json.coder.Coder.<hydra.graph.Graph, T0, hydra.json.model.Value>encodeRecord_encodeField((p0)),
            hydra.lib.lists.Zip.apply(
              (coders),
              (fields))),
          (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>>, hydra.compute.Flow<hydra.graph.Graph, hydra.json.model.Value>>) (maybeFields -> hydra.lib.flows.Pure.apply(new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply((maybeFields)))))));
      }));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T0, hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, T2>>> encodeRecord_matchMaybeTerm(hydra.core.Term fvalue, hydra.compute.Coder<T0, T1, hydra.core.Term, T2> coder_, hydra.core.Name fname, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, T2>>> dflt) {
    return ((fvalue)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, T2>>> otherwise(hydra.core.Term instance) {
        return (dflt);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, T2>>> visit(hydra.core.Term.Maybe opt) {
        return hydra.lib.maybes.Maybe.apply(
          hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, T2>>) (hydra.util.Maybe.<hydra.util.Tuple.Tuple2<String, T2>>nothing())),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, T2>>>>) (v -> hydra.lib.flows.Bind.apply(
            (((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, T2>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, T2>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, T2>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, T2>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, T2>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, T2>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, T2>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, T2>>>) (projected -> projected.encode))))).apply((coder_))).apply((v)),
            (java.util.function.Function<T2, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, T2>>>>) (encoded -> hydra.lib.flows.Pure.apply(hydra.util.Maybe.just((hydra.util.Tuple.Tuple2<String, T2>) ((hydra.util.Tuple.Tuple2<String, T2>) (new hydra.util.Tuple.Tuple2<String, T2>(((fname)).value, (encoded))))))))),
          ((opt)).value);
      }
    });
  }
  
  static <T0> T0 encodeRecord_matchTypeForMaybe(hydra.core.FieldType ft, java.util.function.Function<hydra.core.Type, T0> forMaybe, T0 dflt) {
    return (((ft)).type).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public T0 otherwise(hydra.core.Type instance) {
        return (dflt);
      }
      
      @Override
      public T0 visit(hydra.core.Type.Maybe ot) {
        return ((forMaybe)).apply(((ot)).value);
      }
    });
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T0, hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, T2>>> encodeRecord_encodeField(hydra.util.Tuple.Tuple2<hydra.util.Tuple.Tuple2<hydra.core.FieldType, hydra.compute.Coder<T0, T1, hydra.core.Term, T2>>, hydra.core.Field> coderAndField) {
    hydra.core.Field field = hydra.lib.pairs.Second.apply((coderAndField));
    hydra.core.Name fname = ((field)).name;
    hydra.core.FieldType ft = hydra.lib.pairs.First.apply(hydra.ext.org.json.coder.Coder.encodeRecord_coder((coderAndField)));
    hydra.core.Term fvalue = ((field)).term;
    return hydra.ext.org.json.coder.Coder.encodeRecord_matchTypeForMaybe(
      (ft),
      (java.util.function.Function<hydra.core.Type, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, T2>>>>) (v1 -> hydra.ext.org.json.coder.Coder.encodeRecord_forMaybe(
        hydra.ext.org.json.coder.Coder.encodeRecord_coder_(hydra.ext.org.json.coder.Coder.encodeRecord_coder((coderAndField))),
        (fname),
        (fvalue),
        (v1))),
      hydra.ext.org.json.coder.Coder.encodeRecord_dflt(
        hydra.ext.org.json.coder.Coder.encodeRecord_coder_(hydra.ext.org.json.coder.Coder.encodeRecord_coder((coderAndField))),
        (fname),
        (fvalue)));
  }
  
  static <T0, T1> T0 encodeRecord_coder(hydra.util.Tuple.Tuple2<T0, T1> coderAndField) {
    return hydra.lib.pairs.First.apply((coderAndField));
  }
  
  static <T0, T1> T1 encodeRecord_coder_(hydra.util.Tuple.Tuple2<T0, T1> coder) {
    return hydra.lib.pairs.Second.apply((coder));
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T0, hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, T2>>> encodeRecord_forMaybe(hydra.compute.Coder<T0, T1, hydra.core.Term, T2> coder_, hydra.core.Name fname, hydra.core.Term fvalue, T3 ot) {
    return hydra.ext.org.json.coder.Coder.<T0, T1, T2>encodeRecord_matchMaybeTerm(
      (fvalue),
      (coder_),
      (fname),
      hydra.ext.org.json.coder.Coder.encodeRecord_dflt2(
        (coder_),
        (fname),
        (fvalue)));
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T0, hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, T3>>> encodeRecord_dflt(hydra.compute.Coder<T0, T1, T2, T3> coder_, hydra.core.Name fname, T2 fvalue) {
    return hydra.lib.flows.Bind.apply(
      (((java.util.function.Function<hydra.compute.Coder<T0, T1, T2, T3>, java.util.function.Function<T2, hydra.compute.Flow<T0, T3>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T2, T3>, java.util.function.Function<T2, hydra.compute.Flow<T0, T3>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T2, T3>, java.util.function.Function<T2, hydra.compute.Flow<T0, T3>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T2, T3>, java.util.function.Function<T2, hydra.compute.Flow<T0, T3>>>) (projected -> projected.encode))))).apply((coder_))).apply((fvalue)),
      (java.util.function.Function<T3, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, T3>>>>) (encoded -> hydra.lib.flows.Pure.apply(hydra.util.Maybe.just((hydra.util.Tuple.Tuple2<String, T3>) ((hydra.util.Tuple.Tuple2<String, T3>) (new hydra.util.Tuple.Tuple2<String, T3>(((fname)).value, (encoded))))))));
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T0, hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, T3>>> encodeRecord_dflt2(hydra.compute.Coder<T0, T1, T2, T3> coder_, hydra.core.Name fname, T2 fvalue) {
    return hydra.lib.flows.Bind.apply(
      (((java.util.function.Function<hydra.compute.Coder<T0, T1, T2, T3>, java.util.function.Function<T2, hydra.compute.Flow<T0, T3>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T2, T3>, java.util.function.Function<T2, hydra.compute.Flow<T0, T3>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T2, T3>, java.util.function.Function<T2, hydra.compute.Flow<T0, T3>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, T2, T3>, java.util.function.Function<T2, hydra.compute.Flow<T0, T3>>>) (projected -> projected.encode))))).apply((coder_))).apply((fvalue)),
      (java.util.function.Function<T3, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, T3>>>>) (encoded -> hydra.lib.flows.Pure.apply(hydra.util.Maybe.just((hydra.util.Tuple.Tuple2<String, T3>) ((hydra.util.Tuple.Tuple2<String, T3>) (new hydra.util.Tuple.Tuple2<String, T3>(((fname)).value, (encoded))))))));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.core.Term> decodeRecord(hydra.core.RowType rt, java.util.List<hydra.util.Tuple.Tuple2<hydra.core.FieldType, hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>>> coders, hydra.json.model.Value n) {
    return hydra.ext.org.json.coder.Coder.decodeRecord_result(
      (java.util.function.Function<java.util.Map<String, hydra.json.model.Value>, hydra.compute.Flow<T1, hydra.core.Term>>) (v1 -> hydra.ext.org.json.coder.Coder.<T0, T1>decodeRecord_decodeObjectBody(
        (coders),
        (rt),
        (v1))),
      (n));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.core.Term> decodeRecord_decodeObjectBody(java.util.List<hydra.util.Tuple.Tuple2<hydra.core.FieldType, hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>>> coders, hydra.core.RowType rt, java.util.Map<String, hydra.json.model.Value> m) {
    return hydra.lib.flows.Bind.apply(
      hydra.lib.flows.MapList.apply(
        (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.FieldType, hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>>, hydra.compute.Flow<T1, hydra.core.Field>>) (v1 -> hydra.ext.org.json.coder.Coder.<T0, T1>decodeRecord_decodeField(
          (m),
          (v1))),
        (coders)),
      (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.compute.Flow<T1, hydra.core.Term>>) (fields -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Record(new hydra.core.Record(((rt)).typeName, (fields))))));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, T1> decodeRecord_result(java.util.function.Function<java.util.Map<String, hydra.json.model.Value>, hydra.compute.Flow<T0, T1>> decodeObjectBody, hydra.json.model.Value n) {
    return ((n)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, T1> otherwise(hydra.json.model.Value instance) {
        return hydra.monads.Monads.<T0, T1>unexpected(
          "object",
          hydra.ext.org.json.coder.Coder.showValue((n)));
      }
      
      @Override
      public hydra.compute.Flow<T0, T1> visit(hydra.json.model.Value.Object_ v1) {
        return ((decodeObjectBody)).apply(((v1)).value);
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.core.Field> decodeRecord_decodeField(java.util.Map<String, hydra.json.model.Value> m, hydra.util.Tuple.Tuple2<hydra.core.FieldType, hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>> coder) {
    hydra.json.model.Value defaultValue = new hydra.json.model.Value.Null(true);
    hydra.core.FieldType ft = hydra.lib.pairs.First.apply((coder));
    hydra.core.Name fname = ((ft)).name;
    hydra.json.model.Value jsonValue = hydra.lib.maybes.FromMaybe.apply(
      (defaultValue),
      hydra.lib.maps.Lookup.apply(
        ((fname)).value,
        (m)));
    return hydra.lib.flows.Bind.apply(
      (((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Term>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Term>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Term>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Term>>>) (projected -> projected.decode))))).apply(hydra.ext.org.json.coder.Coder.decodeRecord_coder_((coder)))).apply((jsonValue)),
      (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Field>>) (v -> hydra.lib.flows.Pure.apply(new hydra.core.Field((fname), (v)))));
  }
  
  static <T0, T1> T1 decodeRecord_coder_(hydra.util.Tuple.Tuple2<T0, T1> coder) {
    return hydra.lib.pairs.Second.apply((coder));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>> termCoder(hydra.core.Type typ) {
    java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Literal, String>> matchLiteralString = (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Literal, String>>) (v -> (java.util.function.Function<hydra.core.Literal, String>) (lit -> ((lit)).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public String otherwise(hydra.core.Literal instance) {
        return hydra.show.core.Core.term((v));
      }
      
      @Override
      public String visit(hydra.core.Literal.String_ s) {
        return ((s)).value;
      }
    })));
    java.util.function.Function<hydra.core.Term, String> matchTermLiteral = (java.util.function.Function<hydra.core.Term, String>) (v -> (hydra.rewriting.Rewriting.deannotateTerm((v))).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public String otherwise(hydra.core.Term instance) {
        return hydra.show.core.Core.term((v));
      }
      
      @Override
      public String visit(hydra.core.Term.Literal lit) {
        return (((matchLiteralString)).apply((v))).apply(((lit)).value);
      }
    }));
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType((typ));
    return hydra.ext.org.json.coder.Coder.<T0, T1>termCoder_result(
      (hydra.rewriting.Rewriting::deannotateTerm),
      (hydra.ext.org.json.coder.Coder::readStringStub),
      (hydra.rewriting.Rewriting::deannotateType),
      (hydra.show.core.Core::term),
      (hydra.show.core.Core::type),
      (matchTermLiteral),
      (stripped),
      (typ));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T0, T2> termCoder_encodeLiteral(java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, hydra.compute.Coder<T0, T1, hydra.core.Literal, T2> ac, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, T2> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.<T0, T2>unexpected(
          "literal term",
          ((hydra_show_core_term2)).apply((term)));
      }
      
      @Override
      public hydra.compute.Flow<T0, T2> visit(hydra.core.Term.Literal av) {
        return (((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Literal, T2>, java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<T0, T2>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Literal, T2>, java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<T0, T2>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Literal, T2>, java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<T0, T2>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Literal, T2>, java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<T0, T2>>>) (projected -> projected.encode))))).apply((ac))).apply(((av)).value);
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<T0, hydra.json.model.Value> termCoder_encodeList(java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value> lc, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "list term",
          ((hydra_show_core_term2)).apply((term)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> visit(hydra.core.Term.List els) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.json.model.Value>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.json.model.Value>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.json.model.Value>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.json.model.Value>>>) (projected -> projected.encode))))).apply((lc)),
            ((els)).value),
          (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.compute.Flow<T0, hydra.json.model.Value>>) (encodedEls -> hydra.lib.flows.Pure.apply(new hydra.json.model.Value.Array((encodedEls)))));
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.core.Term> termCoder_decodeList(hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value> lc, hydra.json.model.Value n) {
    return ((n)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> otherwise(hydra.json.model.Value instance) {
        return hydra.monads.Monads.unexpected(
          "sequence",
          hydra.ext.org.json.coder.Coder.showValue((n)));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.json.model.Value.Array nodes) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Term>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Term>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Term>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Term>>>) (projected -> projected.decode))))).apply((lc)),
            ((nodes)).value),
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.compute.Flow<T1, hydra.core.Term>>) (decodedNodes -> hydra.lib.flows.Pure.apply(new hydra.core.Term.List((decodedNodes)))));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.json.model.Value> termCoder_encodeMap(java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>> encodeEntry, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "map term",
          ((hydra_show_core_term2)).apply((term)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> visit(hydra.core.Term.Map m) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (encodeEntry),
            hydra.lib.maps.ToList.apply(((m)).value)),
          (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>, hydra.compute.Flow<T0, hydra.json.model.Value>>) (entries -> hydra.lib.flows.Pure.apply(new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply((entries))))));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> termCoder_decodeMap(java.util.function.Function<hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>> decodeEntry, hydra.json.model.Value n) {
    return ((n)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> otherwise(hydra.json.model.Value instance) {
        return hydra.monads.Monads.unexpected(
          "mapping",
          hydra.ext.org.json.coder.Coder.showValue((n)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.json.model.Value.Object_ m) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (decodeEntry),
            hydra.lib.maps.ToList.apply(((m)).value)),
          (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>, hydra.compute.Flow<T0, hydra.core.Term>>) (entries -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Map(hydra.lib.maps.FromList.apply((entries))))));
      }
    });
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T1, hydra.json.model.Value> termCoder_encodeMaybe(java.util.function.Function<T0, hydra.core.Term> hydra_rewriting_deannotateTerm2, java.util.function.Function<T0, String> hydra_show_core_term2, hydra.compute.Coder<T1, T2, hydra.core.Term, hydra.json.model.Value> maybeElementCoder, T0 maybeTerm) {
    hydra.core.Term strippedMaybeTerm = ((hydra_rewriting_deannotateTerm2)).apply((maybeTerm));
    return ((strippedMaybeTerm)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T1, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "optional term",
          ((hydra_show_core_term2)).apply((maybeTerm)));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.json.model.Value> visit(hydra.core.Term.Maybe maybeContents) {
        return hydra.lib.logic.IfElse.apply(
          hydra.lib.maybes.IsNothing.apply(((maybeContents)).value),
          hydra.lib.flows.Pure.apply(new hydra.json.model.Value.Null(true)),
          hydra.lib.flows.Bind.apply(
            (((java.util.function.Function<hydra.compute.Coder<T1, T2, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.json.model.Value>>>) ((java.util.function.Function<hydra.compute.Coder<T1, T2, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.json.model.Value>>>) ((java.util.function.Function<hydra.compute.Coder<T1, T2, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.json.model.Value>>>) ((java.util.function.Function<hydra.compute.Coder<T1, T2, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.json.model.Value>>>) (projected -> projected.encode))))).apply((maybeElementCoder))).apply(hydra.lib.maybes.FromJust.apply(((maybeContents)).value)),
            (java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.json.model.Value>>) (encodedInner -> hydra.lib.flows.Pure.apply((encodedInner)))));
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.core.Term> termCoder_decodeMaybe(hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value> maybeElementCoder, hydra.json.model.Value jsonVal) {
    return ((jsonVal)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> otherwise(hydra.json.model.Value instance) {
        return hydra.lib.flows.Bind.apply(
          (((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Term>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Term>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Term>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Term>>>) (projected -> projected.decode))))).apply((maybeElementCoder))).apply((jsonVal)),
          (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T1, hydra.core.Term>>) (decodedInner -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Maybe(hydra.util.Maybe.just((decodedInner))))));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.core.Term> visit(hydra.json.model.Value.Null ignored) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())));
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<T0, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>> termCoder_result(java.util.function.Function<hydra.core.Term, hydra.core.Term> hydra_rewriting_deannotateTerm2, java.util.function.Function<String, hydra.core.Term> hydra_ext_org_json_coder_readStringStub2, java.util.function.Function<hydra.core.Type, hydra.core.Type> hydra_rewriting_deannotateType2, java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, java.util.function.Function<hydra.core.Type, String> hydra_show_core_type2, java.util.function.Function<hydra.core.Term, String> matchTermLiteral, hydra.core.Type stripped, hydra.core.Type typ) {
    return ((stripped)).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>> otherwise(hydra.core.Type instance) {
        return hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
          "unsupported type in JSON: ",
          ((hydra_show_core_type2)).apply((typ)))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>> visit(hydra.core.Type.Literal at) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.org.json.coder.Coder.literalJsonCoder(((at)).value),
          (java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Literal, hydra.json.model.Value>, hydra.compute.Flow<T0, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>>>) (ac -> hydra.lib.flows.Pure.apply((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) (new hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>(((java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Literal, hydra.json.model.Value>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.json.model.Value>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.json.model.Value>>) (v2 -> hydra.ext.org.json.coder.Coder.termCoder_encodeLiteral(
            (hydra_show_core_term2),
            (v1),
            (v2))))).apply((ac)), (java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Term>>) (n -> hydra.lib.flows.Bind.apply(
            (((java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Literal, hydra.json.model.Value>, java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Literal>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Literal, hydra.json.model.Value>, java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Literal>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Literal, hydra.json.model.Value>, java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Literal>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Literal, hydra.json.model.Value>, java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Literal>>>) (projected -> projected.decode))))).apply((ac))).apply((n)),
            (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<T1, hydra.core.Term>>) (lit -> hydra.lib.flows.Pure.apply(new hydra.core.Term.Literal((lit))))))))))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>> visit(hydra.core.Type.List lt) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.org.json.coder.Coder.<T0, T1>termCoder(((lt)).value),
          (java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>, hydra.compute.Flow<T0, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>>>) (lc -> hydra.lib.flows.Pure.apply((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) (new hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>(((java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.json.model.Value>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.json.model.Value>>) (v2 -> hydra.ext.org.json.coder.Coder.termCoder_encodeList(
            (hydra_show_core_term2),
            (v1),
            (v2))))).apply((lc)), (java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Term>>) (v1 -> hydra.ext.org.json.coder.Coder.termCoder_decodeList(
            (lc),
            (v1)))))))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>> visit(hydra.core.Type.Map mt) {
        hydra.core.Type kt = (((mt)).value).keys;
        hydra.core.Type vt = (((mt)).value).values;
        return hydra.lib.flows.Bind.apply(
          hydra.ext.org.json.coder.Coder.<T0, T1>termCoder((kt)),
          (java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>, hydra.compute.Flow<T0, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>>>) (kc -> hydra.lib.flows.Bind.apply(
            hydra.ext.org.json.coder.Coder.<T0, T1>termCoder((vt)),
            (java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>, hydra.compute.Flow<T0, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>>>) (vc -> hydra.lib.flows.Bind.apply(
              hydra.monads.Monads.<T0>getState(),
              (java.util.function.Function<T0, hydra.compute.Flow<T0, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>>>) (cx -> {
                Boolean isStringKey = hydra.lib.equality.Equal.apply(
                  ((hydra_rewriting_deannotateType2)).apply((kt)),
                  new hydra.core.Type.Literal(new hydra.core.LiteralType.String_(true)));
                java.util.function.Function<hydra.core.Term, String> toString = (java.util.function.Function<hydra.core.Term, String>) (v -> hydra.lib.logic.IfElse.apply(
                  (isStringKey),
                  ((matchTermLiteral)).apply((v)),
                  ((hydra_show_core_term2)).apply((v))));
                java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>> encodeEntry = (java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>>) (kv -> {
                  hydra.core.Term k = hydra.lib.pairs.First.apply((kv));
                  hydra.core.Term v = hydra.lib.pairs.Second.apply((kv));
                  return hydra.lib.flows.Bind.apply(
                    (((java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.json.model.Value>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.json.model.Value>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.json.model.Value>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.json.model.Value>>>) (projected -> projected.encode))))).apply((vc))).apply((v)),
                    (java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>>) (encodedV -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) ((hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) (new hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>(((toString)).apply((k)), (encodedV)))))));
                });
                java.util.function.Function<String, hydra.core.Term> fromString = (java.util.function.Function<String, hydra.core.Term>) (s -> hydra.lib.logic.IfElse.apply(
                  (isStringKey),
                  new hydra.core.Term.Literal(new hydra.core.Literal.String_((s))),
                  ((hydra_ext_org_json_coder_readStringStub2)).apply((s))));
                return hydra.lib.flows.Pure.apply((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) (new hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>(((java.util.function.Function<java.util.function.Function<hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>, hydra.compute.Flow<hydra.graph.Graph, hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.json.model.Value>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.json.model.Value>>) (v2 -> hydra.ext.org.json.coder.Coder.termCoder_encodeMap(
                  (hydra_show_core_term2),
                  (v1),
                  (v2))))).apply((encodeEntry)), (java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Term>>) (v1 -> hydra.ext.org.json.coder.Coder.<T1>termCoder_decodeMap(
                  (java.util.function.Function<hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>, hydra.compute.Flow<T1, hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term>>>) (v12 -> hydra.ext.org.json.coder.Coder.termCoder_decodeEntry(
                    (fromString),
                    (vc),
                    (v12))),
                  (v1)))))))));
              }))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>> visit(hydra.core.Type.Maybe maybeElementType) {
        return hydra.lib.flows.Bind.apply(
          hydra.ext.org.json.coder.Coder.<T0, T1>termCoder(((maybeElementType)).value),
          (java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>, hydra.compute.Flow<T0, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>>>) (maybeElementCoder -> hydra.lib.flows.Pure.apply((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) (new hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>(((java.util.function.Function<hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.json.model.Value>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.json.model.Value>>) (v2 -> hydra.ext.org.json.coder.Coder.termCoder_encodeMaybe(
            (hydra_rewriting_deannotateTerm2),
            (hydra_show_core_term2),
            (v1),
            (v2))))).apply((maybeElementCoder)), (java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Term>>) (v1 -> hydra.ext.org.json.coder.Coder.termCoder_decodeMaybe(
            (maybeElementCoder),
            (v1)))))))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>> visit(hydra.core.Type.Record rt) {
        return hydra.ext.org.json.coder.Coder.<T0, T1>recordCoder(((rt)).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>> visit(hydra.core.Type.Unit ignored) {
        return hydra.lib.flows.Pure.apply(hydra.ext.org.json.coder.Coder.<hydra.graph.Graph, T1>unitCoder());
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>> visit(hydra.core.Type.Variable name) {
        return hydra.lib.flows.Pure.apply((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>) (new hydra.compute.Coder<hydra.graph.Graph, T1, hydra.core.Term, hydra.json.model.Value>((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<hydra.graph.Graph, hydra.json.model.Value>>) (term -> hydra.lib.flows.Pure.apply(new hydra.json.model.Value.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
          "variable '",
          (((name)).value).value,
          "' for: ",
          ((hydra_show_core_term2)).apply((term))))))), (java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T1, hydra.core.Term>>) (term -> hydra.lib.flows.Fail.apply(hydra.lib.strings.Cat.apply(java.util.List.of(
          "type variable ",
          (((name)).value).value,
          " does not support decoding"))))))))));
      }
    });
  }
  
  static <T0, T1, T2, T3, T4, T5> hydra.compute.Flow<T3, hydra.util.Tuple.Tuple2<T1, T4>> termCoder_decodeEntry(java.util.function.Function<T0, T1> fromString, hydra.compute.Coder<T2, T3, T4, T5> vc, hydra.util.Tuple.Tuple2<T0, T5> kv) {
    T0 k = hydra.lib.pairs.First.apply((kv));
    T5 v = hydra.lib.pairs.Second.apply((kv));
    return hydra.lib.flows.Bind.apply(
      (((java.util.function.Function<hydra.compute.Coder<T2, T3, T4, T5>, java.util.function.Function<T5, hydra.compute.Flow<T3, T4>>>) ((java.util.function.Function<hydra.compute.Coder<T2, T3, T4, T5>, java.util.function.Function<T5, hydra.compute.Flow<T3, T4>>>) ((java.util.function.Function<hydra.compute.Coder<T2, T3, T4, T5>, java.util.function.Function<T5, hydra.compute.Flow<T3, T4>>>) ((java.util.function.Function<hydra.compute.Coder<T2, T3, T4, T5>, java.util.function.Function<T5, hydra.compute.Flow<T3, T4>>>) (projected -> projected.decode))))).apply((vc))).apply((v)),
      (java.util.function.Function<T4, hydra.compute.Flow<T3, hydra.util.Tuple.Tuple2<T1, T4>>>) (decodedV -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<T1, T4>) ((hydra.util.Tuple.Tuple2<T1, T4>) (new hydra.util.Tuple.Tuple2<T1, T4>(((fromString)).apply((k)), (decodedV)))))));
  }
  
  static <T0, T1> hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value> unitCoder() {
    return (hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>) (new hydra.compute.Coder<T0, T1, hydra.core.Term, hydra.json.model.Value>((java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.json.model.Value>>) (v1 -> hydra.ext.org.json.coder.Coder.unitCoder_encodeUnit(
      (hydra.rewriting.Rewriting::deannotateTerm),
      (hydra.show.core.Core::term),
      (v1))), p0 -> hydra.ext.org.json.coder.Coder.<T1>unitCoder_decodeUnit((p0)))))));
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.json.model.Value> unitCoder_encodeUnit(java.util.function.Function<T0, hydra.core.Term> hydra_rewriting_deannotateTerm2, java.util.function.Function<T0, String> hydra_show_core_term2, T0 term) {
    return (((hydra_rewriting_deannotateTerm2)).apply((term))).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T1, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
        return hydra.monads.Monads.unexpected(
          "unit",
          ((hydra_show_core_term2)).apply((term)));
      }
      
      @Override
      public hydra.compute.Flow<T1, hydra.json.model.Value> visit(hydra.core.Term.Unit ignored) {
        return hydra.lib.flows.Pure.apply(new hydra.json.model.Value.Null(true));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Term> unitCoder_decodeUnit(hydra.json.model.Value n) {
    return ((n)).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> otherwise(hydra.json.model.Value instance) {
        return hydra.monads.Monads.unexpected(
          "null",
          hydra.ext.org.json.coder.Coder.showValue((n)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Term> visit(hydra.json.model.Value.Null ignored) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Term.Unit(true));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.json.model.Value> untypedTermToJson(hydra.core.Term term) {
    java.util.function.Function<hydra.core.Literal, hydra.json.model.Value> matchLiteral = (java.util.function.Function<hydra.core.Literal, hydra.json.model.Value>) (lit -> ((lit)).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.json.model.Value visit(hydra.core.Literal.Binary b) {
        return new hydra.json.model.Value.String_(hydra.lib.literals.BinaryToString.apply(((b)).value));
      }
      
      @Override
      public hydra.json.model.Value visit(hydra.core.Literal.Boolean_ b) {
        return new hydra.json.model.Value.Boolean_(((b)).value);
      }
      
      @Override
      public hydra.json.model.Value visit(hydra.core.Literal.Float_ f) {
        return new hydra.json.model.Value.Number_(hydra.literals.Literals.floatValueToBigfloat(((f)).value));
      }
      
      @Override
      public hydra.json.model.Value visit(hydra.core.Literal.Integer_ i) {
        java.math.BigInteger bf = hydra.literals.Literals.integerValueToBigint(((i)).value);
        java.math.BigDecimal f = hydra.lib.literals.BigintToBigfloat.apply((bf));
        return new hydra.json.model.Value.Number_((f));
      }
      
      @Override
      public hydra.json.model.Value visit(hydra.core.Literal.String_ s) {
        return new hydra.json.model.Value.String_(((s)).value);
      }
    }));
    return hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_result(
      (hydra.show.core.Core::elimination),
      p0 -> hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_asRecord((p0)),
      p0 -> p1 -> hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_asVariant(
        (p0),
        (p1)),
      (java.util.function.Function<hydra.core.Field, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>>>) (v1 -> hydra.ext.org.json.coder.Coder.untypedTermToJson_fieldToKeyval(
        p0 -> p1 -> hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_matchTermMaybe(
          (p0),
          (p1)),
        (v1))),
      (hydra.encode.core.Core::type),
      (hydra.show.core.Core::term),
      (matchLiteral),
      (term));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.json.model.Value> untypedTermToJson_unexp(String msg) {
    return hydra.lib.flows.Pure.apply(new hydra.json.model.Value.String_(hydra.lib.strings.Cat2.apply(
      "FAIL: ",
      (msg))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.json.model.Value> untypedTermToJson_asRecord(java.util.List<hydra.core.Field> fields) {
    return hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name(""), (fields))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.json.model.Value> untypedTermToJson_asVariant(String name, hydra.core.Term term) {
    return hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name(""), new hydra.core.Field(new hydra.core.Name((name)), (term)))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.util.Maybe<hydra.json.model.Value>> untypedTermToJson_matchTermMaybe(java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.json.model.Value>>> forTerm, hydra.core.Term t) {
    return ((t)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.util.Maybe<hydra.json.model.Value>> otherwise(hydra.core.Term instance) {
        return hydra.lib.flows.Map.apply(
          (java.util.function.Function<hydra.json.model.Value, hydra.util.Maybe<hydra.json.model.Value>>) ((hydra.lib.maybes.Pure::apply)),
          hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson((t)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.util.Maybe<hydra.json.model.Value>> visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Maybe.apply(
          hydra.lib.flows.Pure.apply((hydra.util.Maybe<hydra.json.model.Value>) (hydra.util.Maybe.<hydra.json.model.Value>nothing())),
          (forTerm),
          ((mt)).value);
      }
    });
  }
  
  static <T0> T0 untypedTermToJson_matchElimination(java.util.function.Function<hydra.core.Elimination, String> hydra_show_core_elimination2, java.util.function.Function<String, T0> unexp, java.util.function.Function<String, java.util.function.Function<hydra.core.Term, T0>> asVariant, hydra.core.Elimination elm) {
    return ((elm)).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public T0 otherwise(hydra.core.Elimination instance) {
        return ((unexp)).apply(hydra.lib.strings.Cat.apply(java.util.List.of(
          "unexpected elimination variant: ",
          ((hydra_show_core_elimination2)).apply((elm)))));
      }
      
      @Override
      public T0 visit(hydra.core.Elimination.Record proj) {
        return (((asVariant)).apply("project")).apply(new hydra.core.Term.Variable((((proj)).value).field));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.json.model.Value> untypedTermToJson_matchFunction(java.util.function.Function<hydra.core.Elimination, String> hydra_show_core_elimination2, java.util.function.Function<hydra.core.Type, hydra.core.Term> hydra_encode_core_type2, java.util.function.Function<String, hydra.compute.Flow<T0, hydra.json.model.Value>> unexp, java.util.function.Function<java.util.List<hydra.core.Field>, hydra.compute.Flow<T0, hydra.json.model.Value>> asRecord, java.util.function.Function<String, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.json.model.Value>>> asVariant, hydra.core.Function f) {
    return ((f)).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> visit(hydra.core.Function.Elimination elm) {
        return ((((java.util.function.Function<java.util.function.Function<String, hydra.compute.Flow<T0, hydra.json.model.Value>>, java.util.function.Function<java.util.function.Function<String, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.json.model.Value>>>, java.util.function.Function<hydra.core.Elimination, hydra.compute.Flow<T0, hydra.json.model.Value>>>>) (v1 -> (java.util.function.Function<java.util.function.Function<String, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.json.model.Value>>>, java.util.function.Function<hydra.core.Elimination, hydra.compute.Flow<T0, hydra.json.model.Value>>>) (v2 -> (java.util.function.Function<hydra.core.Elimination, hydra.compute.Flow<T0, hydra.json.model.Value>>) (v3 -> hydra.ext.org.json.coder.Coder.untypedTermToJson_matchElimination(
          (hydra_show_core_elimination2),
          (v1),
          (v2),
          (v3)))))).apply((unexp))).apply((asVariant))).apply(((elm)).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> visit(hydra.core.Function.Lambda l) {
        return ((asRecord)).apply(java.util.List.of(
          new hydra.core.Field(new hydra.core.Name("parameter"), new hydra.core.Term.Variable((((l)).value).parameter)),
          new hydra.core.Field(new hydra.core.Name("domain"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
            (hydra_encode_core_type2),
            (((l)).value).domain))),
          new hydra.core.Field(new hydra.core.Name("body"), (((l)).value).body)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> visit(hydra.core.Function.Primitive name) {
        return hydra.lib.flows.Pure.apply(new hydra.json.model.Value.String_((((name)).value).value));
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<T0, hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, T1>>> untypedTermToJson_fieldToKeyval(java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.util.Maybe<T1>>>, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.util.Maybe<T1>>>> matchTermMaybe, hydra.core.Field f) {
    return hydra.lib.flows.Bind.apply(
      hydra.ext.org.json.coder.Coder.untypedTermToJson_forTerm(
        (matchTermMaybe),
        ((f)).term),
      (java.util.function.Function<hydra.util.Maybe<T1>, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, T1>>>>) (mjson -> hydra.lib.flows.Pure.apply(hydra.lib.maybes.Map.apply(
        (java.util.function.Function<T1, hydra.util.Tuple.Tuple2<String, T1>>) (j -> (hydra.util.Tuple.Tuple2<String, T1>) ((hydra.util.Tuple.Tuple2<String, T1>) (new hydra.util.Tuple.Tuple2<String, T1>((((f)).name).value, (j))))),
        (mjson)))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.json.model.Value> untypedTermToJson_result(java.util.function.Function<hydra.core.Elimination, String> hydra_show_core_elimination2, java.util.function.Function<java.util.List<hydra.core.Field>, hydra.compute.Flow<T0, hydra.json.model.Value>> asRecord, java.util.function.Function<String, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.json.model.Value>>> asVariant, java.util.function.Function<hydra.core.Field, hydra.compute.Flow<T0, hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>>> fieldToKeyval, java.util.function.Function<hydra.core.Type, hydra.core.Term> hydra_encode_core_type2, java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, java.util.function.Function<hydra.core.Literal, hydra.json.model.Value> matchLiteral, hydra.core.Term term) {
    return ((term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
        return hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_unexp(hydra.lib.strings.Cat.apply(java.util.List.of(
          "unsupported term variant: ",
          ((hydra_show_core_term2)).apply((term)))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> visit(hydra.core.Term.Annotated at) {
        java.util.Map<hydra.core.Name, hydra.core.Term> ann = (((at)).value).annotation;
        hydra.core.Term term1 = (((at)).value).body;
        return hydra.lib.flows.Bind.apply(
          hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson((term1)),
          (java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T0, hydra.json.model.Value>>) (json -> hydra.lib.flows.Bind.apply(
            hydra.lib.flows.MapList.apply(
              p0 -> hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_encodePair((p0)),
              hydra.lib.maps.ToList.apply((ann))),
            (java.util.function.Function<java.util.List<hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>, hydra.compute.Flow<T0, hydra.json.model.Value>>) (pairs -> hydra.lib.flows.Pure.apply(new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(java.util.List.of(
              (hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) ((hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) (new hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>("term", (json)))),
              (hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) ((hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) (new hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>("annotations", new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply((pairs))))))))))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> visit(hydra.core.Term.Application app) {
        return ((asRecord)).apply(java.util.List.of(
          new hydra.core.Field(new hydra.core.Name("function"), (((app)).value).function),
          new hydra.core.Field(new hydra.core.Name("argument"), (((app)).value).argument)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> visit(hydra.core.Term.Function f) {
        return (((((java.util.function.Function<java.util.function.Function<String, hydra.compute.Flow<T0, hydra.json.model.Value>>, java.util.function.Function<java.util.function.Function<java.util.List<hydra.core.Field>, hydra.compute.Flow<T0, hydra.json.model.Value>>, java.util.function.Function<java.util.function.Function<String, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.json.model.Value>>>, java.util.function.Function<hydra.core.Function, hydra.compute.Flow<T0, hydra.json.model.Value>>>>>) (v1 -> (java.util.function.Function<java.util.function.Function<java.util.List<hydra.core.Field>, hydra.compute.Flow<T0, hydra.json.model.Value>>, java.util.function.Function<java.util.function.Function<String, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.json.model.Value>>>, java.util.function.Function<hydra.core.Function, hydra.compute.Flow<T0, hydra.json.model.Value>>>>) (v2 -> (java.util.function.Function<java.util.function.Function<String, java.util.function.Function<hydra.core.Term, hydra.compute.Flow<T0, hydra.json.model.Value>>>, java.util.function.Function<hydra.core.Function, hydra.compute.Flow<T0, hydra.json.model.Value>>>) (v3 -> (java.util.function.Function<hydra.core.Function, hydra.compute.Flow<T0, hydra.json.model.Value>>) (v4 -> hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_matchFunction(
          (hydra_show_core_elimination2),
          (hydra_encode_core_type2),
          (v1),
          (v2),
          (v3),
          (v4))))))).apply(p0 -> hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_unexp((p0)))).apply((asRecord))).apply((asVariant))).apply(((f)).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> visit(hydra.core.Term.Let lt) {
        java.util.List<hydra.core.Binding> bindings = (((lt)).value).bindings;
        hydra.core.Term env = (((lt)).value).body;
        java.util.function.Function<hydra.core.Binding, hydra.core.Field> fromBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Field>) (b -> new hydra.core.Field(((b)).name, ((b)).term));
        return ((asRecord)).apply(java.util.List.of(
          new hydra.core.Field(new hydra.core.Name("bindings"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name(""), hydra.lib.lists.Map.apply(
            (fromBinding),
            (bindings))))),
          new hydra.core.Field(new hydra.core.Name("environment"), (env))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> visit(hydra.core.Term.List terms) {
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            p0 -> hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson((p0)),
            ((terms)).value),
          (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.compute.Flow<T0, hydra.json.model.Value>>) (jsonTerms -> hydra.lib.flows.Pure.apply(new hydra.json.model.Value.Array((jsonTerms)))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> visit(hydra.core.Term.Literal lit) {
        return hydra.lib.flows.Pure.apply(((matchLiteral)).apply(((lit)).value));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Maybe.apply(
          hydra.lib.flows.Pure.apply(new hydra.json.model.Value.Null(true)),
          p0 -> hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson((p0)),
          ((mt)).value);
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> visit(hydra.core.Term.Record r) {
        java.util.List<hydra.core.Field> fields = (((r)).value).fields;
        return hydra.lib.flows.Bind.apply(
          hydra.lib.flows.MapList.apply(
            (fieldToKeyval),
            (fields)),
          (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>>, hydra.compute.Flow<T0, hydra.json.model.Value>>) (keyvals -> hydra.lib.flows.Pure.apply(new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply((keyvals)))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> visit(hydra.core.Term.Set vals) {
        return hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson(new hydra.core.Term.List(hydra.lib.sets.ToList.apply(((vals)).value)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> visit(hydra.core.Term.TypeLambda ta) {
        return ((asRecord)).apply(java.util.List.of(
          new hydra.core.Field(new hydra.core.Name("parameter"), new hydra.core.Term.Variable((((ta)).value).parameter)),
          new hydra.core.Field(new hydra.core.Name("body"), (((ta)).value).body)));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> visit(hydra.core.Term.TypeApplication tt) {
        return ((asRecord)).apply(java.util.List.of(
          new hydra.core.Field(new hydra.core.Name("term"), (((tt)).value).body),
          new hydra.core.Field(new hydra.core.Name("type"), ((hydra_encode_core_type2)).apply((((tt)).value).type))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> visit(hydra.core.Term.Union i) {
        hydra.core.Field field = (((i)).value).field;
        return hydra.lib.logic.IfElse.apply(
          hydra.lib.equality.Equal.apply(
            ((field)).term,
            new hydra.core.Term.Unit(true)),
          hydra.lib.flows.Pure.apply(new hydra.json.model.Value.String_((((field)).name).value)),
          hydra.lib.flows.Bind.apply(
            ((fieldToKeyval)).apply((field)),
            (java.util.function.Function<hydra.util.Maybe<hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>, hydra.compute.Flow<T0, hydra.json.model.Value>>) (mkeyval -> hydra.lib.flows.Pure.apply(new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(hydra.lib.maybes.Maybe.apply(
              (java.util.List<hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>) (java.util.List.<hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>of()),
              (java.util.function.Function<hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>, java.util.List<hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>>) (keyval -> java.util.List.of((keyval))),
              (mkeyval))))))));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> visit(hydra.core.Term.Variable v) {
        return hydra.lib.flows.Pure.apply(new hydra.json.model.Value.String_((((v)).value).value));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.json.model.Value> visit(hydra.core.Term.Wrap wt) {
        return hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson((((wt)).value).body);
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>> untypedTermToJson_encodePair(hydra.util.Tuple.Tuple2<hydra.core.Name, hydra.core.Term> kv) {
    String k = (hydra.lib.pairs.First.apply((kv))).value;
    hydra.core.Term v = hydra.lib.pairs.Second.apply((kv));
    return hydra.lib.flows.Bind.apply(
      hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson((v)),
      (java.util.function.Function<hydra.json.model.Value, hydra.compute.Flow<T0, hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>>>) (json -> hydra.lib.flows.Pure.apply((hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) ((hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>) (new hydra.util.Tuple.Tuple2<String, hydra.json.model.Value>((k), (json)))))));
  }
  
  static <T0, T1> T1 untypedTermToJson_forTerm(java.util.function.Function<java.util.function.Function<T0, T1>, java.util.function.Function<T0, T1>> matchTermMaybe, T0 t) {
    return (((matchTermMaybe)).apply((java.util.function.Function<T0, T1>) (v1 -> hydra.ext.org.json.coder.Coder.<T0, T1>untypedTermToJson_forTerm(
      (matchTermMaybe),
      (v1))))).apply((t));
  }
  
  static hydra.core.Term readStringStub(String s) {
    return new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat2.apply(
      "TODO: read ",
      (s))));
  }
  
  static <T0> String showValue(T0 value) {
    return "TODO: implement showValue";
  }
}
