// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.json.coder;

/**
 * JSON encoding and decoding for Hydra terms
 */
public interface Coder {
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>> jsonCoder(hydra.core.Type typ, hydra.context.Context cx, hydra.graph.Graph g) {
    java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>> mkTermCoder = (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>>) (t -> hydra.ext.org.json.coder.Coder.termCoder(
      t,
      cx,
      g));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.Bimap.apply(
        (java.util.function.Function<String, hydra.context.InContext<hydra.error.Error_>>) (_s -> (hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(_s)), cx))),
        (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>>) (_x -> _x),
        hydra.adapt.modules.Modules.languageAdapter(
          hydra.ext.org.json.language.Language.jsonLanguage(),
          cx,
          g,
          typ)),
      (java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>>) (adapter -> hydra.lib.eithers.Bind.apply(
        (mkTermCoder).apply(((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.core.Type>) (projected -> projected.target))))).apply(adapter)),
        (java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>>) (coder -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>right(hydra.adapt.utils.Utils.composeCoders(
          ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.Type, hydra.core.Type, hydra.core.Term, hydra.core.Term>, hydra.compute.Coder<hydra.core.Term, hydra.core.Term>>) (projected -> projected.coder))))).apply(adapter),
          coder))))))));
  }
  
  static <T0> hydra.util.Either<T0, hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>> literalJsonCoder(hydra.core.LiteralType lt) {
    java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>> decodeBool = (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>) (cx -> (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>) (s -> (s).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal> otherwise(hydra.json.model.Value instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(java.util.List.of(
          "expected boolean, found: ",
          hydra.ext.org.json.coder.Coder.showValue(s))))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal> visit(hydra.json.model.Value.Boolean_ b) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>right(new hydra.core.Literal.Boolean_((b).value))));
      }
    })));
    java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>> decodeFloat = (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>) (cx -> (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>) (s -> (s).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal> otherwise(hydra.json.model.Value instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(java.util.List.of(
          "expected number, found: ",
          hydra.ext.org.json.coder.Coder.showValue(s))))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal> visit(hydra.json.model.Value.Number_ f) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>right(new hydra.core.Literal.Float_(new hydra.core.FloatValue.Bigfloat((f).value)))));
      }
    })));
    java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>> decodeInteger = (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>) (cx -> (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>) (s -> (s).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal> otherwise(hydra.json.model.Value instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(java.util.List.of(
          "expected number, found: ",
          hydra.ext.org.json.coder.Coder.showValue(s))))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal> visit(hydra.json.model.Value.Number_ f) {
        java.math.BigInteger bi = hydra.lib.literals.BigfloatToBigint.apply((f).value);
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>right(new hydra.core.Literal.Integer_(new hydra.core.IntegerValue.Bigint(bi)))));
      }
    })));
    java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>> decodeString = (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>) (cx -> (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>) (s -> (s).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal> otherwise(hydra.json.model.Value instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(java.util.List.of(
          "expected string, found: ",
          hydra.ext.org.json.coder.Coder.showValue(s))))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal> visit(hydra.json.model.Value.String_ s_) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>right(new hydra.core.Literal.String_((s_).value))));
      }
    })));
    hydra.util.Lazy<hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>> encoded = new hydra.util.Lazy<>(() -> (lt).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value> visit(hydra.core.LiteralType.Boolean_ ignored) {
        return (hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>) (new hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>) (cx -> (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (lit -> hydra.lib.eithers.Bind.apply(
          hydra.extract.core.Core.booleanLiteral(
            cx,
            lit),
          (java.util.function.Function<Boolean, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (b -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>right(new hydra.json.model.Value.Boolean_(b)))))))), decodeBool)));
      }
      
      @Override
      public hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value> visit(hydra.core.LiteralType.Float_ ignored) {
        return (hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>) (new hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>) (cx -> (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (lit -> hydra.lib.eithers.Bind.apply(
          hydra.extract.core.Core.floatLiteral(
            cx,
            lit),
          (java.util.function.Function<hydra.core.FloatValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (f -> hydra.lib.eithers.Bind.apply(
            hydra.extract.core.Core.bigfloatValue(
              cx,
              f),
            (java.util.function.Function<java.math.BigDecimal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (bf -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>right(new hydra.json.model.Value.Number_(bf)))))))))), decodeFloat)));
      }
      
      @Override
      public hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value> visit(hydra.core.LiteralType.Integer_ ignored) {
        return (hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>) (new hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>) (cx -> (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (lit -> hydra.lib.eithers.Bind.apply(
          hydra.extract.core.Core.integerLiteral(
            cx,
            lit),
          (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (i -> hydra.lib.eithers.Bind.apply(
            hydra.extract.core.Core.bigintValue(
              cx,
              i),
            (java.util.function.Function<java.math.BigInteger, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (bi -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>right(new hydra.json.model.Value.Number_(hydra.lib.literals.BigintToBigfloat.apply(bi))))))))))), decodeInteger)));
      }
      
      @Override
      public hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value> visit(hydra.core.LiteralType.String_ ignored) {
        return (hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>) (new hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>) (cx -> (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (lit -> hydra.lib.eithers.Bind.apply(
          hydra.extract.core.Core.stringLiteral(
            cx,
            lit),
          (java.util.function.Function<String, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (s -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>right(new hydra.json.model.Value.String_(s)))))))), decodeString)));
      }
    }));
    return (hydra.util.Either<T0, hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>>) ((hydra.util.Either<T0, hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>>) (hydra.util.Either.<T0, hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>>right(encoded.get())));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>> recordCoder(hydra.core.RowType rt, hydra.context.Context cx, hydra.graph.Graph g) {
    java.util.List<hydra.core.FieldType> fields = (rt).fields;
    java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>>> getCoder = (java.util.function.Function<hydra.core.FieldType, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>>>) (f -> hydra.lib.eithers.Bind.apply(
      hydra.ext.org.json.coder.Coder.termCoder(
        (f).type,
        cx,
        g),
      (java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>>>) (coder -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>>right((hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) ((hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) (new hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>(f, coder)))))))));
    return hydra.lib.eithers.Bind.apply(
      hydra.lib.eithers.MapList.apply(
        getCoder,
        fields),
      (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>>) (coders -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>right((hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>) (new hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (term -> hydra.ext.org.json.coder.Coder.encodeRecord(
        coders,
        cx2,
        g,
        term))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>) (cx2 -> (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (val -> hydra.ext.org.json.coder.Coder.decodeRecord(
        rt,
        coders,
        cx2,
        val)))))))))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value> encodeRecord(java.util.List<hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>> coders, hydra.context.Context cx, hydra.graph.Graph graph, hydra.core.Term term) {
    hydra.core.Term stripped = hydra.rewriting.Rewriting.deannotateTerm(term);
    return hydra.lib.eithers.Bind.apply(
      hydra.extract.core.Core.termRecord(
        cx,
        graph,
        stripped),
      (java.util.function.Function<hydra.core.Record, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (record -> {
        java.util.List<hydra.core.Field> fields = (record).fields;
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.util.Pair<hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>, hydra.core.Field>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>>) (v1 -> hydra.ext.org.json.coder.Coder.encodeRecord_encodeField(
              cx,
              v1)),
            hydra.lib.lists.Zip.apply(
              coders,
              fields)),
          (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (maybeFields -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>right(new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(maybeFields))))))));
      }));
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>> encodeRecord_matchMaybeTerm(hydra.context.Context cx, hydra.core.Term fvalue, hydra.compute.Coder<hydra.core.Term, T0> coder_, hydra.core.Name fname, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>> dflt) {
    return (fvalue).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>> otherwise(hydra.core.Term instance) {
        return dflt;
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>> visit(hydra.core.Term.Maybe opt) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>>right((hydra.util.Maybe<hydra.util.Pair<String, T0>>) (hydra.util.Maybe.<hydra.util.Pair<String, T0>>nothing())))),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>>>) (v -> hydra.lib.eithers.Bind.apply(
            ((((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, T0>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, T0>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>>>) (projected -> projected.encode))).apply(coder_)).apply(cx)).apply(v),
            (java.util.function.Function<T0, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>>>) (encoded -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>>right(hydra.util.Maybe.just((hydra.util.Pair<String, T0>) ((hydra.util.Pair<String, T0>) (new hydra.util.Pair<String, T0>((fname).value, encoded)))))))))),
          (opt).value);
      }
    });
  }
  
  static <T0> T0 encodeRecord_matchTypeForMaybe(hydra.core.FieldType ft, java.util.function.Function<hydra.core.Type, T0> forMaybe, T0 dflt) {
    return ((ft).type).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public T0 otherwise(hydra.core.Type instance) {
        return dflt;
      }
      
      @Override
      public T0 visit(hydra.core.Type.Maybe ot) {
        return (forMaybe).apply((ot).value);
      }
    });
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>> encodeRecord_encodeField(hydra.context.Context cx, hydra.util.Pair<hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, T0>>, hydra.core.Field> coderAndField) {
    hydra.util.Lazy<hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, T0>>> coder = new hydra.util.Lazy<>(() -> hydra.ext.org.json.coder.Coder.<T0>encodeRecord_coder(coderAndField));
    hydra.util.Lazy<hydra.compute.Coder<hydra.core.Term, T0>> coder_ = new hydra.util.Lazy<>(() -> hydra.ext.org.json.coder.Coder.<T0>encodeRecord_coder_(coder.get()));
    hydra.util.Lazy<hydra.core.Field> field = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(coderAndField));
    hydra.core.Name fname = (field.get()).name;
    hydra.util.Lazy<hydra.core.FieldType> ft = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(coder.get()));
    hydra.core.Term fvalue = (field.get()).term;
    return hydra.ext.org.json.coder.Coder.encodeRecord_matchTypeForMaybe(
      ft.get(),
      (java.util.function.Function<hydra.core.Type, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>>>) (v1 -> hydra.ext.org.json.coder.Coder.encodeRecord_forMaybe(
        coder_.get(),
        cx,
        fname,
        fvalue,
        v1)),
      hydra.ext.org.json.coder.Coder.<T0>encodeRecord_dflt(
        coder_.get(),
        cx,
        fname,
        fvalue));
  }
  
  static <T0> hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, T0>> encodeRecord_coder(hydra.util.Pair<hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, T0>>, hydra.core.Field> coderAndField) {
    return hydra.lib.pairs.First.apply(coderAndField);
  }
  
  static <T0> hydra.compute.Coder<hydra.core.Term, T0> encodeRecord_coder_(hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, T0>> coder) {
    return hydra.lib.pairs.Second.apply(coder);
  }
  
  static <T0, T1> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>> encodeRecord_forMaybe(hydra.compute.Coder<hydra.core.Term, T0> coder_, hydra.context.Context cx, hydra.core.Name fname, hydra.core.Term fvalue, T1 ot) {
    return hydra.ext.org.json.coder.Coder.<T0>encodeRecord_matchMaybeTerm(
      cx,
      fvalue,
      coder_,
      fname,
      hydra.ext.org.json.coder.Coder.<T0>encodeRecord_dflt2(
        coder_,
        cx,
        fname,
        fvalue));
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>> encodeRecord_dflt(hydra.compute.Coder<hydra.core.Term, T0> coder_, hydra.context.Context cx, hydra.core.Name fname, hydra.core.Term fvalue) {
    return hydra.lib.eithers.Bind.apply(
      ((((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, T0>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, T0>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>>>) (projected -> projected.encode))).apply(coder_)).apply(cx)).apply(fvalue),
      (java.util.function.Function<T0, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>>>) (encoded -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>>right(hydra.util.Maybe.just((hydra.util.Pair<String, T0>) ((hydra.util.Pair<String, T0>) (new hydra.util.Pair<String, T0>((fname).value, encoded)))))))));
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>> encodeRecord_dflt2(hydra.compute.Coder<hydra.core.Term, T0> coder_, hydra.context.Context cx, hydra.core.Name fname, hydra.core.Term fvalue) {
    return hydra.lib.eithers.Bind.apply(
      ((((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, T0>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, T0>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>>>) (projected -> projected.encode))).apply(coder_)).apply(cx)).apply(fvalue),
      (java.util.function.Function<T0, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>>>) (encoded -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Maybe<hydra.util.Pair<String, T0>>>right(hydra.util.Maybe.just((hydra.util.Pair<String, T0>) ((hydra.util.Pair<String, T0>) (new hydra.util.Pair<String, T0>((fname).value, encoded)))))))));
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> decodeRecord(hydra.core.RowType rt, java.util.List<hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>> coders, hydra.context.Context cx, hydra.json.model.Value n) {
    java.util.function.Function<java.util.Map<String, hydra.json.model.Value>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>> decodeObjectBody = (java.util.function.Function<java.util.Map<String, hydra.json.model.Value>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (m -> {
      java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>> decodeField = (java.util.function.Function<hydra.util.Pair<hydra.core.FieldType, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>>) (coder -> {
        hydra.util.Lazy<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>> coder_ = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(coder));
        hydra.json.model.Value defaultValue = new hydra.json.model.Value.Null();
        hydra.util.Lazy<hydra.core.FieldType> ft = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(coder));
        hydra.core.Name fname = (ft.get()).name;
        hydra.util.Lazy<hydra.json.model.Value> jsonValue = new hydra.util.Lazy<>(() -> hydra.lib.maybes.FromMaybe.applyLazy(
          () -> defaultValue,
          hydra.lib.maps.Lookup.apply(
            (fname).value,
            m)));
        return hydra.lib.eithers.Bind.apply(
          ((((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>>) (projected -> projected.decode))).apply(coder_.get())).apply(cx)).apply(jsonValue.get()),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>>) (v -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Field>right(new hydra.core.Field(fname, v))))));
      });
      return hydra.lib.eithers.Bind.apply(
        hydra.lib.eithers.MapList.apply(
          decodeField,
          coders),
        (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (fields -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Record(new hydra.core.Record((rt).typeName, fields)))))));
    });
    return (n).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> otherwise(hydra.json.model.Value instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(java.util.List.of(
          "expected object, found: ",
          hydra.ext.org.json.coder.Coder.showValue(n))))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.json.model.Value.Object_ v1) {
        return (decodeObjectBody).apply((v1).value);
      }
    });
  }
  
  static hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>> termCoder(hydra.core.Type typ, hydra.context.Context cx, hydra.graph.Graph g) {
    java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>> decodeList = (java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>>) (lc -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>) (cx2 -> (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (n -> (n).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> otherwise(hydra.json.model.Value instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(java.util.List.of(
          "expected sequence, found: ",
          hydra.ext.org.json.coder.Coder.showValue(n))))), cx2)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.json.model.Value.Array nodes) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (node -> ((((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>>) (projected -> projected.decode))).apply(lc)).apply(cx2)).apply(node)),
            (nodes).value),
          (java.util.function.Function<java.util.List<hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (decodedNodes -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.List(decodedNodes))))));
      }
    }))));
    java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>> decodeMaybe = (java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>>) (maybeElementCoder -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>) (cx2 -> (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (jsonVal -> (jsonVal).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> otherwise(hydra.json.model.Value instance) {
        return hydra.lib.eithers.Bind.apply(
          ((((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>>) (projected -> projected.decode))).apply(maybeElementCoder)).apply(cx2)).apply(jsonVal),
          (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (decodedInner -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Maybe(hydra.util.Maybe.just(decodedInner)))))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.json.model.Value.Null ignored) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Maybe((hydra.util.Maybe<hydra.core.Term>) (hydra.util.Maybe.<hydra.core.Term>nothing())))));
      }
    }))));
    java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>> encodeList = (java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>>) (lc -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(java.util.List.of(
          "expected list term, found: ",
          hydra.show.core.Core.term(term))))), cx2)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value> visit(hydra.core.Term.List els) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (el -> ((((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>>) (projected -> projected.encode))).apply(lc)).apply(cx2)).apply(el)),
            (els).value),
          (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (encodedEls -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>right(new hydra.json.model.Value.Array(encodedEls))))));
      }
    }))));
    java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>> encodeMaybe = (java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>>) (maybeElementCoder -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (maybeTerm -> {
      hydra.core.Term strippedMaybeTerm = hydra.rewriting.Rewriting.deannotateTerm(maybeTerm);
      return (strippedMaybeTerm).accept(new hydra.core.Term.PartialVisitor<>() {
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
          return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(java.util.List.of(
            "expected optional term, found: ",
            hydra.show.core.Core.term(maybeTerm))))), cx2)))));
        }
        
        @Override
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value> visit(hydra.core.Term.Maybe maybeContents) {
          return hydra.lib.logic.IfElse.lazy(
            hydra.lib.maybes.IsNothing.apply((maybeContents).value),
            () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>right(new hydra.json.model.Value.Null()))),
            () -> hydra.lib.eithers.Bind.apply(
              ((((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>>) (projected -> projected.encode))).apply(maybeElementCoder)).apply(cx2)).apply(hydra.lib.maybes.FromJust.apply((maybeContents).value)),
              (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (encodedInner -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>right(encodedInner))))));
        }
      });
    })));
    java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Literal, String>> matchLiteralString = (java.util.function.Function<hydra.core.Term, java.util.function.Function<hydra.core.Literal, String>>) (v -> (java.util.function.Function<hydra.core.Literal, String>) (lit -> (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public String otherwise(hydra.core.Literal instance) {
        return hydra.show.core.Core.term(v);
      }
      
      @Override
      public String visit(hydra.core.Literal.String_ s) {
        return (s).value;
      }
    })));
    java.util.function.Function<hydra.core.Term, String> matchTermLiteral = (java.util.function.Function<hydra.core.Term, String>) (v -> (hydra.rewriting.Rewriting.deannotateTerm(v)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public String otherwise(hydra.core.Term instance) {
        return hydra.show.core.Core.term(v);
      }
      
      @Override
      public String visit(hydra.core.Term.Literal lit) {
        return ((matchLiteralString).apply(v)).apply((lit).value);
      }
    }));
    hydra.core.Type stripped = hydra.rewriting.Rewriting.deannotateType(typ);
    hydra.util.Lazy<hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>> result = new hydra.util.Lazy<>(() -> (stripped).accept(new hydra.core.Type.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>> otherwise(hydra.core.Type instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(java.util.List.of(
          "unsupported type in JSON: ",
          hydra.show.core.Core.type(typ))))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>> visit(hydra.core.Type.Literal at) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.org.json.coder.Coder.literalJsonCoder((at).value),
          (java.util.function.Function<hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>>) (ac -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>right((hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>) (new hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (v2 -> hydra.ext.org.json.coder.Coder.termCoder_encodeLiteral(
            hydra.show.core.Core::term,
            ac,
            v1,
            v2))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>) (cx2 -> (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (n -> hydra.lib.eithers.Bind.apply(
            ((((java.util.function.Function<hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.Literal, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>>) (projected -> projected.decode))).apply(ac)).apply(cx2)).apply(n),
            (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (lit -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Literal(lit))))))))))))))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>> visit(hydra.core.Type.List lt) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.org.json.coder.Coder.termCoder(
            (lt).value,
            cx,
            g),
          (java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>>) (lc -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>right((hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>) (new hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (v2 -> (((encodeList).apply(lc)).apply(v1)).apply(v2))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (v2 -> (((decodeList).apply(lc)).apply(v1)).apply(v2)))))))))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>> visit(hydra.core.Type.Map mt) {
        hydra.core.Type kt = ((mt).value).keys;
        hydra.core.Type vt = ((mt).value).values;
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.org.json.coder.Coder.termCoder(
            kt,
            cx,
            g),
          (java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>>) (kc -> hydra.lib.eithers.Bind.apply(
            hydra.ext.org.json.coder.Coder.termCoder(
              vt,
              cx,
              g),
            (java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>>) (vc -> {
              hydra.util.Lazy<Boolean> isStringKey = new hydra.util.Lazy<>(() -> hydra.lib.equality.Equal.apply(
                hydra.rewriting.Rewriting.deannotateType(kt),
                new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())));
              java.util.function.Function<String, hydra.core.Term> fromString = (java.util.function.Function<String, hydra.core.Term>) (s -> hydra.lib.logic.IfElse.lazy(
                isStringKey.get(),
                () -> new hydra.core.Term.Literal(new hydra.core.Literal.String_(s)),
                () -> hydra.ext.org.json.coder.Coder.readStringStub(s)));
              java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.util.Pair<String, hydra.json.model.Value>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>> decodeEntry = (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.util.Pair<String, hydra.json.model.Value>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>>) (cx2 -> (java.util.function.Function<hydra.util.Pair<String, hydra.json.model.Value>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>) (kv -> {
                hydra.util.Lazy<String> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(kv));
                hydra.util.Lazy<hydra.json.model.Value> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(kv));
                return hydra.lib.eithers.Bind.apply(
                  ((((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>>) (projected -> projected.decode))).apply(vc)).apply(cx2)).apply(v.get()),
                  (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>) (decodedV -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>right((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) ((hydra.util.Pair<hydra.core.Term, hydra.core.Term>) (new hydra.util.Pair<hydra.core.Term, hydra.core.Term>((fromString).apply(k.get()), decodedV))))))));
              }));
              java.util.function.Function<hydra.core.Term, String> toString = (java.util.function.Function<hydra.core.Term, String>) (v -> hydra.lib.logic.IfElse.lazy(
                isStringKey.get(),
                () -> (matchTermLiteral).apply(v),
                () -> hydra.show.core.Core.term(v)));
              java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<String, hydra.json.model.Value>>>> encodeEntry = (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<String, hydra.json.model.Value>>>>) (cx2 -> (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<String, hydra.json.model.Value>>>) (kv -> {
                hydra.util.Lazy<hydra.core.Term> k = new hydra.util.Lazy<>(() -> hydra.lib.pairs.First.apply(kv));
                hydra.util.Lazy<hydra.core.Term> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(kv));
                return hydra.lib.eithers.Bind.apply(
                  ((((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>>) (projected -> projected.encode))).apply(vc)).apply(cx2)).apply(v.get()),
                  (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<String, hydra.json.model.Value>>>) (encodedV -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<String, hydra.json.model.Value>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<String, hydra.json.model.Value>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<String, hydra.json.model.Value>>right((hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>((toString).apply(k.get()), encodedV))))))));
              }));
              return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>right((hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>) (new hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>) (cx2 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (term -> (term).accept(new hydra.core.Term.PartialVisitor<>() {
                @Override
                public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
                  return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(java.util.List.of(
                    "expected map term, found: ",
                    hydra.show.core.Core.term(term))))), cx2)))));
                }
                
                @Override
                public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value> visit(hydra.core.Term.Map m) {
                  return hydra.lib.eithers.Bind.apply(
                    hydra.lib.eithers.MapList.apply(
                      (java.util.function.Function<hydra.util.Pair<hydra.core.Term, hydra.core.Term>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<String, hydra.json.model.Value>>>) (entry -> ((encodeEntry).apply(cx2)).apply(entry)),
                      hydra.lib.maps.ToList.apply((m).value)),
                    (java.util.function.Function<java.util.List<hydra.util.Pair<String, hydra.json.model.Value>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (entries -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>right(new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(entries)))))));
                }
              }))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>) (cx2 -> (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (n -> (n).accept(new hydra.json.model.Value.PartialVisitor<>() {
                @Override
                public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> otherwise(hydra.json.model.Value instance) {
                  return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(java.util.List.of(
                    "expected mapping, found: ",
                    hydra.ext.org.json.coder.Coder.showValue(n))))), cx2)))));
                }
                
                @Override
                public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.json.model.Value.Object_ m) {
                  return hydra.lib.eithers.Bind.apply(
                    hydra.lib.eithers.MapList.apply(
                      (java.util.function.Function<hydra.util.Pair<String, hydra.json.model.Value>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.util.Pair<hydra.core.Term, hydra.core.Term>>>) (entry -> ((decodeEntry).apply(cx2)).apply(entry)),
                      hydra.lib.maps.ToList.apply((m).value)),
                    (java.util.function.Function<java.util.List<hydra.util.Pair<hydra.core.Term, hydra.core.Term>>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (entries -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Map(hydra.lib.maps.FromList.apply(entries)))))));
                }
              })))))))));
            }))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>> visit(hydra.core.Type.Maybe maybeElementType) {
        return hydra.lib.eithers.Bind.apply(
          hydra.ext.org.json.coder.Coder.termCoder(
            (maybeElementType).value,
            cx,
            g),
          (java.util.function.Function<hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>>) (maybeElementCoder -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>right((hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>) (new hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (v2 -> (((encodeMaybe).apply(maybeElementCoder)).apply(v1)).apply(v2))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>) (v1 -> (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (v2 -> (((decodeMaybe).apply(maybeElementCoder)).apply(v1)).apply(v2)))))))))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>> visit(hydra.core.Type.Record rt) {
        return hydra.ext.org.json.coder.Coder.recordCoder(
          (rt).value,
          cx,
          g);
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>> visit(hydra.core.Type.Unit ignored) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>right(hydra.ext.org.json.coder.Coder.unitCoder())));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>> visit(hydra.core.Type.Variable name) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>>right((hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>) (new hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>) (_cx -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (term -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>right(new hydra.json.model.Value.String_(hydra.lib.strings.Cat.apply(java.util.List.of(
          "variable '",
          ((name).value).value,
          "' for: ",
          hydra.show.core.Core.term(term))))))))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>) (cx2 -> (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (_term -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(java.util.List.of(
          "type variable ",
          ((name).value).value,
          " does not support decoding")))), cx2)))))))))))));
      }
    }));
    return result.get();
  }
  
  static <T0> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0> termCoder_encodeLiteral(java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, hydra.compute.Coder<hydra.core.Literal, T0> ac, hydra.context.Context cx, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0> otherwise(hydra.core.Term instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, T0>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(java.util.List.of(
          "expected literal term, found: ",
          (hydra_show_core_term2).apply(term))))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0> visit(hydra.core.Term.Literal av) {
        return ((((java.util.function.Function<hydra.compute.Coder<hydra.core.Literal, T0>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.Literal, T0>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, T0>>>>) (projected -> projected.encode))).apply(ac)).apply(cx)).apply((av).value);
      }
    });
  }
  
  static hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value> unitCoder() {
    java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>> decodeUnit = (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>>) (cx -> (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>>) (n -> (n).accept(new hydra.json.model.Value.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> otherwise(hydra.json.model.Value instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(java.util.List.of(
          "expected null, found: ",
          hydra.ext.org.json.coder.Coder.showValue(n))))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term> visit(hydra.json.model.Value.Null ignored) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Term>right(new hydra.core.Term.Unit())));
      }
    })));
    java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>> encodeUnit = (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>>) (cx -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>>) (term -> (hydra.rewriting.Rewriting.deannotateTerm(term)).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat.apply(java.util.List.of(
          "expected unit, found: ",
          hydra.show.core.Core.term(term))))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value> visit(hydra.core.Term.Unit ignored) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.json.model.Value>right(new hydra.json.model.Value.Null())));
      }
    })));
    return (hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>) ((hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>) (new hydra.compute.Coder<hydra.core.Term, hydra.json.model.Value>(encodeUnit, decodeUnit)));
  }
  
  static <T0> hydra.util.Either<T0, hydra.json.model.Value> untypedTermToJson(hydra.core.Term term) {
    java.util.function.Function<hydra.core.Literal, hydra.json.model.Value> matchLiteral = (java.util.function.Function<hydra.core.Literal, hydra.json.model.Value>) (lit -> (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.json.model.Value visit(hydra.core.Literal.Binary b) {
        return new hydra.json.model.Value.String_(hydra.lib.literals.BinaryToString.apply((b).value));
      }
      
      @Override
      public hydra.json.model.Value visit(hydra.core.Literal.Boolean_ b) {
        return new hydra.json.model.Value.Boolean_((b).value);
      }
      
      @Override
      public hydra.json.model.Value visit(hydra.core.Literal.Float_ f) {
        return new hydra.json.model.Value.Number_(hydra.literals.Literals.floatValueToBigfloat((f).value));
      }
      
      @Override
      public hydra.json.model.Value visit(hydra.core.Literal.Integer_ i) {
        java.math.BigInteger bf = hydra.literals.Literals.integerValueToBigint((i).value);
        java.math.BigDecimal f = hydra.lib.literals.BigintToBigfloat.apply(bf);
        return new hydra.json.model.Value.Number_(f);
      }
      
      @Override
      public hydra.json.model.Value visit(hydra.core.Literal.String_ s) {
        return new hydra.json.model.Value.String_((s).value);
      }
    }));
    java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.json.model.Value>> recurse = p0 -> hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_recurse(p0);
    return hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_result(
      hydra.show.core.Core::elimination,
      (java.util.function.Function<java.util.List<hydra.core.Field>, hydra.util.Either<T0, hydra.json.model.Value>>) (v1 -> hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_asRecord(
        recurse,
        v1)),
      (java.util.function.Function<String, java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.json.model.Value>>>) (v1 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.json.model.Value>>) (v2 -> hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_asVariant(
        recurse,
        v1,
        v2))),
      (java.util.function.Function<hydra.core.Field, hydra.util.Either<T0, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>>) (v1 -> hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_fieldToKeyval(
        (java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.util.Maybe<hydra.json.model.Value>>>, java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.util.Maybe<hydra.json.model.Value>>>>) (v12 -> (java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.util.Maybe<hydra.json.model.Value>>>) (v2 -> hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_matchTermMaybe(
          recurse,
          v12,
          v2))),
        v1)),
      hydra.encode.core.Core::type,
      hydra.show.core.Core::term,
      matchLiteral,
      recurse,
      term);
  }
  
  static <T0> hydra.util.Either<T0, hydra.json.model.Value> untypedTermToJson_recurse(hydra.core.Term t) {
    return hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson(t);
  }
  
  static <T1> hydra.util.Either<T1, hydra.json.model.Value> untypedTermToJson_unexp(String msg) {
    return (hydra.util.Either<T1, hydra.json.model.Value>) ((hydra.util.Either<T1, hydra.json.model.Value>) (hydra.util.Either.<T1, hydra.json.model.Value>right(new hydra.json.model.Value.String_(hydra.lib.strings.Cat2.apply(
      "FAIL: ",
      msg)))));
  }
  
  static <T0> hydra.util.Either<T0, hydra.json.model.Value> untypedTermToJson_asRecord(java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.json.model.Value>> recurse, java.util.List<hydra.core.Field> fields) {
    return (recurse).apply(new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name(""), fields)));
  }
  
  static <T0> hydra.util.Either<T0, hydra.json.model.Value> untypedTermToJson_asVariant(java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.json.model.Value>> recurse, String name, hydra.core.Term term) {
    return (recurse).apply(new hydra.core.Term.Union(new hydra.core.Injection(new hydra.core.Name(""), new hydra.core.Field(new hydra.core.Name(name), term))));
  }
  
  static <T0> hydra.util.Either<T0, hydra.util.Maybe<hydra.json.model.Value>> untypedTermToJson_matchTermMaybe(java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.json.model.Value>> recurse, java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.util.Maybe<hydra.json.model.Value>>> forTerm, hydra.core.Term t) {
    return (t).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T0, hydra.util.Maybe<hydra.json.model.Value>> otherwise(hydra.core.Term instance) {
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.json.model.Value, hydra.util.Maybe<hydra.json.model.Value>>) (hydra.lib.maybes.Pure::apply),
          (recurse).apply(t));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.util.Maybe<hydra.json.model.Value>> visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> (hydra.util.Either<T0, hydra.util.Maybe<hydra.json.model.Value>>) ((hydra.util.Either<T0, hydra.util.Maybe<hydra.json.model.Value>>) (hydra.util.Either.<T0, hydra.util.Maybe<hydra.json.model.Value>>right((hydra.util.Maybe<hydra.json.model.Value>) (hydra.util.Maybe.<hydra.json.model.Value>nothing())))),
          forTerm,
          (mt).value);
      }
    });
  }
  
  static <T1> T1 untypedTermToJson_matchElimination(java.util.function.Function<hydra.core.Elimination, String> hydra_show_core_elimination2, java.util.function.Function<String, T1> unexp, java.util.function.Function<String, java.util.function.Function<hydra.core.Term, T1>> asVariant, hydra.core.Elimination elm) {
    return (elm).accept(new hydra.core.Elimination.PartialVisitor<>() {
      @Override
      public T1 otherwise(hydra.core.Elimination instance) {
        return (unexp).apply(hydra.lib.strings.Cat.apply(java.util.List.of(
          "unexpected elimination variant: ",
          (hydra_show_core_elimination2).apply(elm))));
      }
      
      @Override
      public T1 visit(hydra.core.Elimination.Record proj) {
        return ((asVariant).apply("project")).apply(new hydra.core.Term.Variable(((proj).value).field));
      }
    });
  }
  
  static <T1> hydra.util.Either<T1, hydra.json.model.Value> untypedTermToJson_matchFunction(java.util.function.Function<hydra.core.Elimination, String> hydra_show_core_elimination2, java.util.function.Function<hydra.core.Type, hydra.core.Term> hydra_encode_core_type2, java.util.function.Function<String, hydra.util.Either<T1, hydra.json.model.Value>> unexp, java.util.function.Function<java.util.List<hydra.core.Field>, hydra.util.Either<T1, hydra.json.model.Value>> asRecord, java.util.function.Function<String, java.util.function.Function<hydra.core.Term, hydra.util.Either<T1, hydra.json.model.Value>>> asVariant, hydra.core.Function f) {
    return (f).accept(new hydra.core.Function.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T1, hydra.json.model.Value> visit(hydra.core.Function.Elimination elm) {
        return hydra.ext.org.json.coder.Coder.untypedTermToJson_matchElimination(
          hydra_show_core_elimination2,
          unexp,
          asVariant,
          (elm).value);
      }
      
      @Override
      public hydra.util.Either<T1, hydra.json.model.Value> visit(hydra.core.Function.Lambda l) {
        return (asRecord).apply(java.util.List.of(
          new hydra.core.Field(new hydra.core.Name("parameter"), new hydra.core.Term.Variable(((l).value).parameter)),
          new hydra.core.Field(new hydra.core.Name("domain"), new hydra.core.Term.Maybe(hydra.lib.maybes.Map.apply(
            hydra_encode_core_type2,
            ((l).value).domain))),
          new hydra.core.Field(new hydra.core.Name("body"), ((l).value).body)));
      }
      
      @Override
      public hydra.util.Either<T1, hydra.json.model.Value> visit(hydra.core.Function.Primitive name) {
        return (hydra.util.Either<T1, hydra.json.model.Value>) ((hydra.util.Either<T1, hydra.json.model.Value>) (hydra.util.Either.<T1, hydra.json.model.Value>right(new hydra.json.model.Value.String_(((name).value).value))));
      }
    });
  }
  
  static <T0> hydra.util.Either<T0, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>> untypedTermToJson_fieldToKeyval(java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.util.Maybe<hydra.json.model.Value>>>, java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.util.Maybe<hydra.json.model.Value>>>> matchTermMaybe, hydra.core.Field f) {
    return hydra.lib.eithers.Bind.apply(
      hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_forTerm(
        matchTermMaybe,
        (f).term),
      (java.util.function.Function<hydra.util.Maybe<hydra.json.model.Value>, hydra.util.Either<T0, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>>) (mjson -> (hydra.util.Either<T0, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>) ((hydra.util.Either<T0, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>) (hydra.util.Either.<T0, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>right(hydra.lib.maybes.Map.apply(
        (java.util.function.Function<hydra.json.model.Value, hydra.util.Pair<String, hydra.json.model.Value>>) (j -> (hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>(((f).name).value, j)))),
        mjson))))));
  }
  
  static <T0> hydra.util.Either<T0, hydra.json.model.Value> untypedTermToJson_result(java.util.function.Function<hydra.core.Elimination, String> hydra_show_core_elimination2, java.util.function.Function<java.util.List<hydra.core.Field>, hydra.util.Either<T0, hydra.json.model.Value>> asRecord, java.util.function.Function<String, java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.json.model.Value>>> asVariant, java.util.function.Function<hydra.core.Field, hydra.util.Either<T0, hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>> fieldToKeyval, java.util.function.Function<hydra.core.Type, hydra.core.Term> hydra_encode_core_type2, java.util.function.Function<hydra.core.Term, String> hydra_show_core_term2, java.util.function.Function<hydra.core.Literal, hydra.json.model.Value> matchLiteral, java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.json.model.Value>> recurse, hydra.core.Term term) {
    return (term).accept(new hydra.core.Term.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> otherwise(hydra.core.Term instance) {
        return hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_unexp(hydra.lib.strings.Cat.apply(java.util.List.of(
          "unsupported term variant: ",
          (hydra_show_core_term2).apply(term))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.Term.Annotated at) {
        java.util.Map<hydra.core.Name, hydra.core.Term> ann = ((at).value).annotation;
        hydra.core.Term term1 = ((at).value).body;
        return hydra.lib.eithers.Bind.apply(
          (recurse).apply(term1),
          (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<T0, hydra.json.model.Value>>) (json -> hydra.lib.eithers.Bind.apply(
            hydra.lib.eithers.MapList.apply(
              (java.util.function.Function<hydra.util.Pair<hydra.core.Name, hydra.core.Term>, hydra.util.Either<T0, hydra.util.Pair<String, hydra.json.model.Value>>>) (v1 -> hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_encodePair(
                recurse,
                v1)),
              hydra.lib.maps.ToList.apply(ann)),
            (java.util.function.Function<java.util.List<hydra.util.Pair<String, hydra.json.model.Value>>, hydra.util.Either<T0, hydra.json.model.Value>>) (pairs -> (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(java.util.List.of(
              (hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>("term", json))),
              (hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>("annotations", new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(pairs)))))))))))))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.Term.Application app) {
        return (asRecord).apply(java.util.List.of(
          new hydra.core.Field(new hydra.core.Name("function"), ((app).value).function),
          new hydra.core.Field(new hydra.core.Name("argument"), ((app).value).argument)));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.Term.Function f) {
        return hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_matchFunction(
          hydra_show_core_elimination2,
          hydra_encode_core_type2,
          p0 -> hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_unexp(p0),
          asRecord,
          asVariant,
          (f).value);
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.Term.Let lt) {
        java.util.List<hydra.core.Binding> bindings = ((lt).value).bindings;
        hydra.core.Term env = ((lt).value).body;
        java.util.function.Function<hydra.core.Binding, hydra.core.Field> fromBinding = (java.util.function.Function<hydra.core.Binding, hydra.core.Field>) (b -> new hydra.core.Field((b).name, (b).term));
        return (asRecord).apply(java.util.List.of(
          new hydra.core.Field(new hydra.core.Name("bindings"), new hydra.core.Term.Record(new hydra.core.Record(new hydra.core.Name(""), hydra.lib.lists.Map.apply(
            fromBinding,
            bindings)))),
          new hydra.core.Field(new hydra.core.Name("environment"), env)));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.Term.List terms) {
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            recurse,
            (terms).value),
          (java.util.function.Function<java.util.List<hydra.json.model.Value>, hydra.util.Either<T0, hydra.json.model.Value>>) (jsonTerms -> (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.Array(jsonTerms))))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.Term.Literal lit) {
        return (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right((matchLiteral).apply((lit).value))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.Term.Maybe mt) {
        return hydra.lib.maybes.Maybe.applyLazy(
          () -> (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.Null()))),
          recurse,
          (mt).value);
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.Term.Record r) {
        java.util.List<hydra.core.Field> fields = ((r).value).fields;
        return hydra.lib.eithers.Bind.apply(
          hydra.lib.eithers.MapList.apply(
            fieldToKeyval,
            fields),
          (java.util.function.Function<java.util.List<hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>>, hydra.util.Either<T0, hydra.json.model.Value>>) (keyvals -> (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(hydra.lib.maybes.Cat.apply(keyvals))))))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.Term.Set vals) {
        return (recurse).apply(new hydra.core.Term.List(hydra.lib.sets.ToList.apply((vals).value)));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.Term.TypeLambda ta) {
        return (asRecord).apply(java.util.List.of(
          new hydra.core.Field(new hydra.core.Name("parameter"), new hydra.core.Term.Variable(((ta).value).parameter)),
          new hydra.core.Field(new hydra.core.Name("body"), ((ta).value).body)));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.Term.TypeApplication tt) {
        return (asRecord).apply(java.util.List.of(
          new hydra.core.Field(new hydra.core.Name("term"), ((tt).value).body),
          new hydra.core.Field(new hydra.core.Name("type"), (hydra_encode_core_type2).apply(((tt).value).type))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.Term.Union i) {
        hydra.core.Field field = ((i).value).field;
        return hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            (field).term,
            new hydra.core.Term.Unit()),
          () -> (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.String_(((field).name).value)))),
          () -> hydra.lib.eithers.Bind.apply(
            (fieldToKeyval).apply(field),
            (java.util.function.Function<hydra.util.Maybe<hydra.util.Pair<String, hydra.json.model.Value>>, hydra.util.Either<T0, hydra.json.model.Value>>) (mkeyval -> (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.Object_(hydra.lib.maps.FromList.apply(hydra.lib.maybes.Maybe.applyLazy(
              () -> (java.util.List<hydra.util.Pair<String, hydra.json.model.Value>>) (java.util.List.<hydra.util.Pair<String, hydra.json.model.Value>>of()),
              (java.util.function.Function<hydra.util.Pair<String, hydra.json.model.Value>, java.util.List<hydra.util.Pair<String, hydra.json.model.Value>>>) (keyval -> java.util.List.of(keyval)),
              mkeyval)))))))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.Term.Variable v) {
        return (hydra.util.Either<T0, hydra.json.model.Value>) ((hydra.util.Either<T0, hydra.json.model.Value>) (hydra.util.Either.<T0, hydra.json.model.Value>right(new hydra.json.model.Value.String_(((v).value).value))));
      }
      
      @Override
      public hydra.util.Either<T0, hydra.json.model.Value> visit(hydra.core.Term.Wrap wt) {
        return (recurse).apply(((wt).value).body);
      }
    });
  }
  
  static <T0> hydra.util.Either<T0, hydra.util.Pair<String, hydra.json.model.Value>> untypedTermToJson_encodePair(java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.json.model.Value>> recurse, hydra.util.Pair<hydra.core.Name, hydra.core.Term> kv) {
    hydra.util.Lazy<String> k = new hydra.util.Lazy<>(() -> (hydra.lib.pairs.First.apply(kv)).value);
    hydra.util.Lazy<hydra.core.Term> v = new hydra.util.Lazy<>(() -> hydra.lib.pairs.Second.apply(kv));
    return hydra.lib.eithers.Bind.apply(
      (recurse).apply(v.get()),
      (java.util.function.Function<hydra.json.model.Value, hydra.util.Either<T0, hydra.util.Pair<String, hydra.json.model.Value>>>) (json -> (hydra.util.Either<T0, hydra.util.Pair<String, hydra.json.model.Value>>) ((hydra.util.Either<T0, hydra.util.Pair<String, hydra.json.model.Value>>) (hydra.util.Either.<T0, hydra.util.Pair<String, hydra.json.model.Value>>right((hydra.util.Pair<String, hydra.json.model.Value>) ((hydra.util.Pair<String, hydra.json.model.Value>) (new hydra.util.Pair<String, hydra.json.model.Value>(k.get(), json))))))));
  }
  
  static <T0> hydra.util.Either<T0, hydra.util.Maybe<hydra.json.model.Value>> untypedTermToJson_forTerm(java.util.function.Function<java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.util.Maybe<hydra.json.model.Value>>>, java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.util.Maybe<hydra.json.model.Value>>>> matchTermMaybe, hydra.core.Term t) {
    return ((matchTermMaybe).apply((java.util.function.Function<hydra.core.Term, hydra.util.Either<T0, hydra.util.Maybe<hydra.json.model.Value>>>) (v1 -> hydra.ext.org.json.coder.Coder.<T0>untypedTermToJson_forTerm(
      matchTermMaybe,
      v1)))).apply(t);
  }
  
  static hydra.core.Term readStringStub(String s) {
    return new hydra.core.Term.Literal(new hydra.core.Literal.String_(hydra.lib.strings.Cat2.apply(
      "TODO: read ",
      s)));
  }
  
  static <T0> String showValue(T0 value) {
    return "TODO: implement showValue";
  }
}
