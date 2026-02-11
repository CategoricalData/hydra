// Note: this is an automatically generated file. Do not edit.

package hydra.adapt.literals;

/**
 * Adapter framework for literal types and terms
 */
public interface Literals {
  static hydra.util.Comparison comparePrecision(hydra.util.Precision p1, hydra.util.Precision p2) {
    return (p1).accept(new hydra.util.Precision.PartialVisitor<>() {
      @Override
      public hydra.util.Comparison visit(hydra.util.Precision.Arbitrary ignored) {
        return (p2).accept(new hydra.util.Precision.PartialVisitor<>() {
          @Override
          public hydra.util.Comparison visit(hydra.util.Precision.Arbitrary _2) {
            return new hydra.util.Comparison.EqualTo();
          }
          
          @Override
          public hydra.util.Comparison visit(hydra.util.Precision.Bits _2) {
            return new hydra.util.Comparison.GreaterThan();
          }
        });
      }
      
      @Override
      public hydra.util.Comparison visit(hydra.util.Precision.Bits b1) {
        return (p2).accept(new hydra.util.Precision.PartialVisitor<>() {
          @Override
          public hydra.util.Comparison visit(hydra.util.Precision.Arbitrary ignored) {
            return new hydra.util.Comparison.LessThan();
          }
          
          @Override
          public hydra.util.Comparison visit(hydra.util.Precision.Bits b2) {
            return hydra.lib.logic.IfElse.lazy(
              hydra.lib.equality.Lt.apply(
                (b1).value,
                (b2).value),
              () -> new hydra.util.Comparison.LessThan(),
              () -> new hydra.util.Comparison.GreaterThan());
          }
        });
      }
    });
  }
  
  static hydra.core.FloatValue convertFloatValue(hydra.core.FloatType target, hydra.core.FloatValue fv) {
    java.util.function.Function<hydra.core.FloatValue, java.math.BigDecimal> decoder = (java.util.function.Function<hydra.core.FloatValue, java.math.BigDecimal>) (fv2 -> (fv2).accept(new hydra.core.FloatValue.PartialVisitor<>() {
      @Override
      public java.math.BigDecimal visit(hydra.core.FloatValue.Bigfloat d) {
        return (d).value;
      }
      
      @Override
      public java.math.BigDecimal visit(hydra.core.FloatValue.Float32 f) {
        return hydra.lib.literals.Float32ToBigfloat.apply((f).value);
      }
      
      @Override
      public java.math.BigDecimal visit(hydra.core.FloatValue.Float64 d) {
        return hydra.lib.literals.Float64ToBigfloat.apply((d).value);
      }
    }));
    java.util.function.Function<java.math.BigDecimal, hydra.core.FloatValue> encoder = (java.util.function.Function<java.math.BigDecimal, hydra.core.FloatValue>) (d -> (target).accept(new hydra.core.FloatType.PartialVisitor<>() {
      @Override
      public hydra.core.FloatValue visit(hydra.core.FloatType.Bigfloat ignored) {
        return new hydra.core.FloatValue.Bigfloat(d);
      }
      
      @Override
      public hydra.core.FloatValue visit(hydra.core.FloatType.Float32 ignored) {
        return new hydra.core.FloatValue.Float32(hydra.lib.literals.BigfloatToFloat32.apply(d));
      }
      
      @Override
      public hydra.core.FloatValue visit(hydra.core.FloatType.Float64 ignored) {
        return new hydra.core.FloatValue.Float64(hydra.lib.literals.BigfloatToFloat64.apply(d));
      }
    }));
    return (encoder).apply((decoder).apply(fv));
  }
  
  static hydra.core.IntegerValue convertIntegerValue(hydra.core.IntegerType target, hydra.core.IntegerValue iv) {
    java.util.function.Function<hydra.core.IntegerValue, java.math.BigInteger> decoder = (java.util.function.Function<hydra.core.IntegerValue, java.math.BigInteger>) (iv2 -> (iv2).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Bigint v) {
        return (v).value;
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Int8 v) {
        return hydra.lib.literals.Int8ToBigint.apply((v).value);
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Int16 v) {
        return hydra.lib.literals.Int16ToBigint.apply((v).value);
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Int32 v) {
        return hydra.lib.literals.Int32ToBigint.apply((v).value);
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Int64 v) {
        return hydra.lib.literals.Int64ToBigint.apply((v).value);
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Uint8 v) {
        return hydra.lib.literals.Uint8ToBigint.apply((v).value);
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Uint16 v) {
        return hydra.lib.literals.Uint16ToBigint.apply((v).value);
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Uint32 v) {
        return hydra.lib.literals.Uint32ToBigint.apply((v).value);
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Uint64 v) {
        return hydra.lib.literals.Uint64ToBigint.apply((v).value);
      }
    }));
    java.util.function.Function<java.math.BigInteger, hydra.core.IntegerValue> encoder = (java.util.function.Function<java.math.BigInteger, hydra.core.IntegerValue>) (d -> (target).accept(new hydra.core.IntegerType.PartialVisitor<>() {
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Bigint ignored) {
        return new hydra.core.IntegerValue.Bigint(d);
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Int8 ignored) {
        return new hydra.core.IntegerValue.Int8(hydra.lib.literals.BigintToInt8.apply(d));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Int16 ignored) {
        return new hydra.core.IntegerValue.Int16(hydra.lib.literals.BigintToInt16.apply(d));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Int32 ignored) {
        return new hydra.core.IntegerValue.Int32(hydra.lib.literals.BigintToInt32.apply(d));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Int64 ignored) {
        return new hydra.core.IntegerValue.Int64(hydra.lib.literals.BigintToInt64.apply(d));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Uint8 ignored) {
        return new hydra.core.IntegerValue.Uint8(hydra.lib.literals.BigintToUint8.apply(d));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Uint16 ignored) {
        return new hydra.core.IntegerValue.Uint16(hydra.lib.literals.BigintToUint16.apply(d));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Uint32 ignored) {
        return new hydra.core.IntegerValue.Uint32(hydra.lib.literals.BigintToUint32.apply(d));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Uint64 ignored) {
        return new hydra.core.IntegerValue.Uint64(hydra.lib.literals.BigintToUint64.apply(d));
      }
    }));
    return (encoder).apply((decoder).apply(iv));
  }
  
  static String disclaimer(Boolean lossy, String source, String target) {
    return hydra.lib.strings.Cat.apply(java.util.List.of(
      "replace ",
      source,
      " with ",
      target,
      hydra.lib.logic.IfElse.lazy(
        lossy,
        () -> " (lossy)",
        () -> "")));
  }
  
  static <T0> hydra.compute.Flow<hydra.coders.AdapterContext, hydra.compute.Adapter<T0, T0, hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>> literalAdapter(hydra.core.LiteralType lt) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.coders.AdapterContext>getState(),
      (java.util.function.Function<hydra.coders.AdapterContext, hydra.compute.Flow<hydra.coders.AdapterContext, hydra.compute.Adapter<T0, T0, hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>) (cx -> {
        java.util.function.Function<hydra.core.LiteralType, Boolean> supported = (java.util.function.Function<hydra.core.LiteralType, Boolean>) (v1 -> hydra.adapt.utils.Utils.literalTypeIsSupported(
          ((cx).language).constraints,
          v1));
        return hydra.adapt.utils.Utils.chooseAdapter(
          (java.util.function.Function<hydra.core.LiteralType, hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T0, T0, hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>>) (v1 -> hydra.adapt.literals.Literals.<T0>literalAdapter_alts(
            hydra.show.core.Core::literal,
            v1)),
          supported,
          hydra.show.core.Core::literalType,
          hydra.show.core.Core::literalType,
          lt);
      }));
  }
  
  static <T0, T1, T2, T3> hydra.compute.Flow<T1, java.util.List<hydra.compute.Adapter<T2, T3, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> literalAdapter_forBinary(T0 t) {
    return hydra.lib.flows.Pure.apply(java.util.List.of((hydra.compute.Adapter<T2, T3, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T2, T3, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T2, T3, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T2, T3, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T2, T3, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T2, T3, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) (new hydra.compute.Adapter<T2, T3, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>(false, t, new hydra.core.LiteralType.String_(), hydra.adapt.literals.Literals.<T2, T3>literalAdapter_step4())))))))));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> literalAdapter_forBoolean(T0 t) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.coders.AdapterContext>getState(),
      (java.util.function.Function<hydra.coders.AdapterContext, hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>>) (cx -> {
        hydra.coders.LanguageConstraints constraints = ((cx).language).constraints;
        hydra.util.Lazy<Boolean> hasIntegers = new hydra.util.Lazy<>(() -> hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply((constraints).integerTypes)));
        hydra.util.Lazy<Boolean> hasStrings = new hydra.util.Lazy<>(() -> hydra.lib.sets.Member.apply(
          new hydra.variants.LiteralVariant.String_(),
          (constraints).literalVariants));
        return hydra.lib.logic.IfElse.lazy(
          hasIntegers.get(),
          () -> hydra.adapt.literals.Literals.<T0, T1, T2>literalAdapter_withIntegers2(t),
          () -> hydra.lib.logic.IfElse.lazy(
            hasStrings.get(),
            () -> hydra.lib.flows.Pure.apply(hydra.adapt.literals.Literals.<T0, T1, T2>literalAdapter_withStrings(t)),
            () -> hydra.lib.flows.Fail.apply("no alternatives available for boolean encoding")));
      }));
  }
  
  static <T0, T1> hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> literalAdapter_forFloat(java.util.function.Function<hydra.core.Literal, String> hydra_show_core_literal2, T0 t, hydra.core.FloatType ft) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.coders.AdapterContext>getState(),
      (java.util.function.Function<hydra.coders.AdapterContext, hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>>) (cx -> {
        hydra.coders.LanguageConstraints constraints = ((cx).language).constraints;
        hydra.util.Lazy<Boolean> hasFloats = new hydra.util.Lazy<>(() -> hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply((constraints).floatTypes)));
        return hydra.lib.logic.IfElse.lazy(
          hasFloats.get(),
          () -> hydra.adapt.literals.Literals.<T0, T1>literalAdapter_withFloats(
            ft,
            hydra_show_core_literal2,
            t),
          () -> hydra.lib.flows.Fail.apply("no float types available"));
      }));
  }
  
  static <T0, T1> hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> literalAdapter_forInteger(java.util.function.Function<hydra.core.Literal, String> hydra_show_core_literal2, T0 t, hydra.core.IntegerType it) {
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.coders.AdapterContext>getState(),
      (java.util.function.Function<hydra.coders.AdapterContext, hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>>) (cx -> {
        hydra.coders.LanguageConstraints constraints = ((cx).language).constraints;
        hydra.util.Lazy<Boolean> hasIntegers = new hydra.util.Lazy<>(() -> hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply((constraints).integerTypes)));
        return hydra.lib.logic.IfElse.lazy(
          hasIntegers.get(),
          () -> hydra.adapt.literals.Literals.<T0, T1>literalAdapter_withIntegers(
            hydra_show_core_literal2,
            it,
            t),
          () -> hydra.lib.flows.Fail.apply("no integer types available"));
      }));
  }
  
  static <T0> hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T0, T0, hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> literalAdapter_alts(java.util.function.Function<hydra.core.Literal, String> hydra_show_core_literal2, hydra.core.LiteralType t) {
    return (t).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T0, T0, hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> visit(hydra.core.LiteralType.Binary ignored) {
        return hydra.adapt.literals.Literals.literalAdapter_forBinary(t);
      }
      
      @Override
      public hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T0, T0, hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> visit(hydra.core.LiteralType.Boolean_ ignored) {
        return hydra.adapt.literals.Literals.literalAdapter_forBoolean(t);
      }
      
      @Override
      public hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T0, T0, hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> visit(hydra.core.LiteralType.Float_ ft) {
        return (((java.util.function.Function<hydra.core.LiteralType, java.util.function.Function<hydra.core.FloatType, hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T0, T0, hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>>>) (v1 -> (java.util.function.Function<hydra.core.FloatType, hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T0, T0, hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>>) (v2 -> hydra.adapt.literals.Literals.literalAdapter_forFloat(
          hydra_show_core_literal2,
          v1,
          v2)))).apply(t)).apply((ft).value);
      }
      
      @Override
      public hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T0, T0, hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> visit(hydra.core.LiteralType.Integer_ it) {
        return (((java.util.function.Function<hydra.core.LiteralType, java.util.function.Function<hydra.core.IntegerType, hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T0, T0, hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>>>) (v1 -> (java.util.function.Function<hydra.core.IntegerType, hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T0, T0, hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>>) (v2 -> hydra.adapt.literals.Literals.literalAdapter_forInteger(
          hydra_show_core_literal2,
          v1,
          v2)))).apply(t)).apply((it).value);
      }
      
      @Override
      public hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T0, T0, hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> visit(hydra.core.LiteralType.String_ ignored) {
        return hydra.lib.flows.Fail.apply("no substitute for the literal string type");
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> literalAdapter_withIntegers(java.util.function.Function<hydra.core.Literal, String> hydra_show_core_literal2, hydra.core.IntegerType it, T0 t) {
    return hydra.lib.flows.Bind.apply(
      hydra.adapt.literals.Literals.<T1, T1>integerAdapter(it),
      (java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>>) (adapter -> hydra.lib.flows.Pure.apply(java.util.List.of((hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) (new hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>(((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, Boolean>) (projected -> projected.isLossy))))))).apply(adapter), t, new hydra.core.LiteralType.Integer_(((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) (projected -> projected.target))))))).apply(adapter)), hydra.adapt.literals.Literals.literalAdapter_step(
        hydra_show_core_literal2,
        adapter))))))))))));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T0, hydra.core.Literal> literalAdapter_adapt(java.util.function.Function<hydra.core.Literal, String> hydra_show_core_literal2, hydra.compute.Adapter<T0, T0, T1, T2, hydra.core.IntegerValue, hydra.core.IntegerValue> adapter, hydra.coders.CoderDirection dir, hydra.core.Literal lit) {
    return (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Literal> otherwise(hydra.core.Literal instance) {
        return hydra.monads.Monads.unexpected(
          "integer literal",
          (hydra_show_core_literal2).apply(lit));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Literal> visit(hydra.core.Literal.Integer_ iv) {
        return hydra.lib.flows.Map.apply(
          (java.util.function.Function<hydra.core.IntegerValue, hydra.core.Literal>) (x -> new hydra.core.Literal.Integer_(x)),
          hydra.adapt.utils.Utils.encodeDecode(
            dir,
            ((java.util.function.Function<hydra.compute.Adapter<T0, T0, T1, T2, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.compute.Coder<T0, T0, hydra.core.IntegerValue, hydra.core.IntegerValue>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T0, T1, T2, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.compute.Coder<T0, T0, hydra.core.IntegerValue, hydra.core.IntegerValue>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T0, T1, T2, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.compute.Coder<T0, T0, hydra.core.IntegerValue, hydra.core.IntegerValue>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T0, T1, T2, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.compute.Coder<T0, T0, hydra.core.IntegerValue, hydra.core.IntegerValue>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T0, T1, T2, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.compute.Coder<T0, T0, hydra.core.IntegerValue, hydra.core.IntegerValue>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T0, T1, T2, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.compute.Coder<T0, T0, hydra.core.IntegerValue, hydra.core.IntegerValue>>) (projected -> projected.coder))))))).apply(adapter),
            (iv).value));
      }
    });
  }
  
  static <T0, T1, T2> hydra.compute.Coder<T0, T0, hydra.core.Literal, hydra.core.Literal> literalAdapter_step(java.util.function.Function<hydra.core.Literal, String> hydra_show_core_literal2, hydra.compute.Adapter<T0, T0, T1, T2, hydra.core.IntegerValue, hydra.core.IntegerValue> adapter) {
    return hydra.adapt.utils.Utils.bidirectional(((java.util.function.Function<hydra.compute.Adapter<T0, T0, T1, T2, hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<T0, hydra.core.Literal>>>>) (v1 -> (java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<T0, hydra.core.Literal>>>) (v2 -> (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<T0, hydra.core.Literal>>) (v3 -> hydra.adapt.literals.Literals.<T0, T1, T2>literalAdapter_adapt(
      hydra_show_core_literal2,
      v1,
      v2,
      v3))))).apply(adapter));
  }
  
  static <T0, T1> hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> literalAdapter_withFloats(hydra.core.FloatType ft, java.util.function.Function<hydra.core.Literal, String> hydra_show_core_literal2, T0 t) {
    return hydra.lib.flows.Bind.apply(
      hydra.adapt.literals.Literals.<T1, T1>floatAdapter(ft),
      (java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>>) (adapter -> hydra.lib.flows.Pure.apply(java.util.List.of((hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) (new hydra.compute.Adapter<T1, T1, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>(((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, Boolean>) (projected -> projected.isLossy))))))).apply(adapter), t, new hydra.core.LiteralType.Float_(((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.core.FloatType>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.core.FloatType>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.core.FloatType>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.core.FloatType>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.core.FloatType>) ((java.util.function.Function<hydra.compute.Adapter<T1, T1, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.core.FloatType>) (projected -> projected.target))))))).apply(adapter)), hydra.adapt.literals.Literals.literalAdapter_step2(
        hydra_show_core_literal2,
        adapter))))))))))));
  }
  
  static <T0, T1, T2> hydra.compute.Flow<T0, hydra.core.Literal> literalAdapter_adapt2(java.util.function.Function<hydra.core.Literal, String> hydra_show_core_literal2, hydra.compute.Adapter<T0, T0, T1, T2, hydra.core.FloatValue, hydra.core.FloatValue> adapter, hydra.coders.CoderDirection dir, hydra.core.Literal l) {
    return (l).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Literal> otherwise(hydra.core.Literal instance) {
        return hydra.monads.Monads.unexpected(
          "floating-point literal",
          (hydra_show_core_literal2).apply(l));
      }
      
      @Override
      public hydra.compute.Flow<T0, hydra.core.Literal> visit(hydra.core.Literal.Float_ fv) {
        return hydra.lib.flows.Map.apply(
          (java.util.function.Function<hydra.core.FloatValue, hydra.core.Literal>) (x -> new hydra.core.Literal.Float_(x)),
          hydra.adapt.utils.Utils.encodeDecode(
            dir,
            ((java.util.function.Function<hydra.compute.Adapter<T0, T0, T1, T2, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.compute.Coder<T0, T0, hydra.core.FloatValue, hydra.core.FloatValue>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T0, T1, T2, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.compute.Coder<T0, T0, hydra.core.FloatValue, hydra.core.FloatValue>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T0, T1, T2, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.compute.Coder<T0, T0, hydra.core.FloatValue, hydra.core.FloatValue>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T0, T1, T2, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.compute.Coder<T0, T0, hydra.core.FloatValue, hydra.core.FloatValue>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T0, T1, T2, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.compute.Coder<T0, T0, hydra.core.FloatValue, hydra.core.FloatValue>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T0, T1, T2, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.compute.Coder<T0, T0, hydra.core.FloatValue, hydra.core.FloatValue>>) (projected -> projected.coder))))))).apply(adapter),
            (fv).value));
      }
    });
  }
  
  static <T0, T1, T2> hydra.compute.Coder<T0, T0, hydra.core.Literal, hydra.core.Literal> literalAdapter_step2(java.util.function.Function<hydra.core.Literal, String> hydra_show_core_literal2, hydra.compute.Adapter<T0, T0, T1, T2, hydra.core.FloatValue, hydra.core.FloatValue> adapter) {
    return hydra.adapt.utils.Utils.bidirectional(((java.util.function.Function<hydra.compute.Adapter<T0, T0, T1, T2, hydra.core.FloatValue, hydra.core.FloatValue>, java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<T0, hydra.core.Literal>>>>) (v1 -> (java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<T0, hydra.core.Literal>>>) (v2 -> (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<T0, hydra.core.Literal>>) (v3 -> hydra.adapt.literals.Literals.<T0, T1, T2>literalAdapter_adapt2(
      hydra_show_core_literal2,
      v1,
      v2,
      v3))))).apply(adapter));
  }
  
  static <T0, T1> hydra.compute.Flow<T0, hydra.core.Literal> literalAdapter_matchBoolean(hydra.compute.Coder<T0, T1, hydra.core.IntegerValue, hydra.core.IntegerValue> step_, hydra.core.Literal lit) {
    return (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Literal> visit(hydra.core.Literal.Boolean_ bv) {
        return hydra.lib.flows.Bind.apply(
          (((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<T0, hydra.core.IntegerValue>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<T0, hydra.core.IntegerValue>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<T0, hydra.core.IntegerValue>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<T0, hydra.core.IntegerValue>>>) (projected -> projected.encode))))).apply(step_)).apply(new hydra.core.IntegerValue.Uint8(hydra.lib.logic.IfElse.lazy(
            (bv).value,
            () -> (short) (1),
            () -> (short) (0)))),
          (java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<T0, hydra.core.Literal>>) (iv -> hydra.lib.flows.Pure.apply(new hydra.core.Literal.Integer_(iv))));
      }
    });
  }
  
  static <T0, T1> hydra.compute.Flow<T1, hydra.core.Literal> literalAdapter_matchInteger(hydra.compute.Coder<T0, T1, hydra.core.IntegerValue, hydra.core.IntegerValue> step_, hydra.core.Literal lit) {
    java.util.function.Function<hydra.core.IntegerValue, hydra.core.Literal> forValue = (java.util.function.Function<hydra.core.IntegerValue, hydra.core.Literal>) (val -> (val).accept(new hydra.core.IntegerValue.PartialVisitor<>() {
      @Override
      public hydra.core.Literal visit(hydra.core.IntegerValue.Uint8 v) {
        return new hydra.core.Literal.Boolean_(hydra.lib.equality.Equal.apply(
          (v).value,
          (short) (1)));
      }
    }));
    return (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T1, hydra.core.Literal> visit(hydra.core.Literal.Integer_ iv) {
        return hydra.lib.flows.Bind.apply(
          (((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<T1, hydra.core.IntegerValue>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<T1, hydra.core.IntegerValue>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<T1, hydra.core.IntegerValue>>>) ((java.util.function.Function<hydra.compute.Coder<T0, T1, hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<T1, hydra.core.IntegerValue>>>) (projected -> projected.decode))))).apply(step_)).apply((iv).value),
          (java.util.function.Function<hydra.core.IntegerValue, hydra.compute.Flow<T1, hydra.core.Literal>>) (val -> hydra.lib.flows.Pure.apply((forValue).apply(val))));
      }
    });
  }
  
  static <T0, T1, T2> hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> literalAdapter_withIntegers2(T0 t) {
    return hydra.lib.flows.Bind.apply(
      hydra.adapt.literals.Literals.<T1, T2>integerAdapter(new hydra.core.IntegerType.Uint8()),
      (java.util.function.Function<hydra.compute.Adapter<T1, T2, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>>) (adapter -> ((java.util.function.Function<hydra.compute.Adapter<T1, T2, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>>) (v1 -> hydra.adapt.literals.Literals.literalAdapter_withAdapter(
        t,
        v1))).apply(adapter)));
  }
  
  static <T0, T1, T2> java.util.List<hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>> literalAdapter_withStrings(T0 t) {
    return java.util.List.of((hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) (new hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>(false, t, new hydra.core.LiteralType.String_(), (hydra.compute.Coder<T1, T2, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Coder<T1, T2, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Coder<T1, T2, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Coder<T1, T2, hydra.core.Literal, hydra.core.Literal>) (new hydra.compute.Coder<T1, T2, hydra.core.Literal, hydra.core.Literal>(p0 -> hydra.adapt.literals.Literals.<T1>literalAdapter_encode(p0), p0 -> hydra.adapt.literals.Literals.<T2>literalAdapter_decode(p0))))))))))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Literal> literalAdapter_encode(hydra.core.Literal lit) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.<T0>booleanLiteral(lit),
      (java.util.function.Function<Boolean, hydra.compute.Flow<T0, hydra.core.Literal>>) (b -> hydra.lib.flows.Pure.apply(new hydra.core.Literal.String_(hydra.lib.logic.IfElse.lazy(
        b,
        () -> "true",
        () -> "false")))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Literal> literalAdapter_decode(hydra.core.Literal lit) {
    return hydra.lib.flows.Bind.apply(
      hydra.extract.core.Core.<T0>stringLiteral(lit),
      (java.util.function.Function<String, hydra.compute.Flow<T0, hydra.core.Literal>>) (s -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          s,
          "true"),
        () -> hydra.lib.flows.Pure.apply(new hydra.core.Literal.Boolean_(true)),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            s,
            "false"),
          () -> hydra.lib.flows.Pure.apply(new hydra.core.Literal.Boolean_(false)),
          () -> hydra.monads.Monads.unexpected(
            "boolean literal",
            s)))));
  }
  
  static <T0, T1, T2, T3, T4> hydra.compute.Flow<T4, java.util.List<hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> literalAdapter_withAdapter(T0 t, hydra.compute.Adapter<T1, T2, T3, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue> adapter) {
    return hydra.lib.flows.Pure.apply(java.util.List.of((hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) (new hydra.compute.Adapter<T1, T2, T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>(false, t, new hydra.core.LiteralType.Integer_(((java.util.function.Function<hydra.compute.Adapter<T1, T2, T3, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) ((java.util.function.Function<hydra.compute.Adapter<T1, T2, T3, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) ((java.util.function.Function<hydra.compute.Adapter<T1, T2, T3, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) ((java.util.function.Function<hydra.compute.Adapter<T1, T2, T3, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) ((java.util.function.Function<hydra.compute.Adapter<T1, T2, T3, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) ((java.util.function.Function<hydra.compute.Adapter<T1, T2, T3, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) (projected -> projected.target))))))).apply(adapter)), hydra.adapt.literals.Literals.<T1, T2>literalAdapter_step3(hydra.adapt.literals.Literals.literalAdapter_step_(adapter)))))))))));
  }
  
  static <T0, T1, T2, T3, T4, T5> hydra.compute.Coder<T0, T1, T4, T5> literalAdapter_step_(hydra.compute.Adapter<T0, T1, T2, T3, T4, T5> adapter) {
    return ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, hydra.compute.Coder<T0, T1, T4, T5>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, hydra.compute.Coder<T0, T1, T4, T5>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, hydra.compute.Coder<T0, T1, T4, T5>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, hydra.compute.Coder<T0, T1, T4, T5>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, hydra.compute.Coder<T0, T1, T4, T5>>) ((java.util.function.Function<hydra.compute.Adapter<T0, T1, T2, T3, T4, T5>, hydra.compute.Coder<T0, T1, T4, T5>>) (projected -> projected.coder))))))).apply(adapter);
  }
  
  static <T0, T1> hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.core.Literal> literalAdapter_step3(hydra.compute.Coder<T0, T1, hydra.core.IntegerValue, hydra.core.IntegerValue> step_) {
    return (hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.core.Literal>) (new hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.core.Literal>((java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<T0, hydra.core.Literal>>) (v1 -> hydra.adapt.literals.Literals.<T0, T1>literalAdapter_matchBoolean(
      step_,
      v1)), (java.util.function.Function<hydra.core.Literal, hydra.compute.Flow<T1, hydra.core.Literal>>) (v1 -> hydra.adapt.literals.Literals.<T0, T1>literalAdapter_matchInteger(
      step_,
      v1)))))));
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Literal> literalAdapter_matchBinary(hydra.core.Literal lit) {
    return (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Literal> visit(hydra.core.Literal.Binary b) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Literal.String_(hydra.lib.literals.BinaryToString.apply((b).value)));
      }
    });
  }
  
  static <T0> hydra.compute.Flow<T0, hydra.core.Literal> literalAdapter_matchString(hydra.core.Literal lit) {
    return (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.compute.Flow<T0, hydra.core.Literal> visit(hydra.core.Literal.String_ s) {
        return hydra.lib.flows.Pure.apply(new hydra.core.Literal.Binary(hydra.lib.literals.StringToBinary.apply((s).value)));
      }
    });
  }
  
  static <T0, T1> hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.core.Literal> literalAdapter_step4() {
    return (hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.core.Literal>) (new hydra.compute.Coder<T0, T1, hydra.core.Literal, hydra.core.Literal>(p0 -> hydra.adapt.literals.Literals.<T0>literalAdapter_matchBinary(p0), p0 -> hydra.adapt.literals.Literals.<T1>literalAdapter_matchString(p0))))));
  }
  
  static <T0, T1> hydra.compute.Flow<hydra.coders.AdapterContext, hydra.compute.Adapter<T0, T1, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>> floatAdapter(hydra.core.FloatType ft) {
    java.util.function.Function<hydra.core.FloatType, java.util.List<hydra.core.FloatType>> altTypes = (java.util.function.Function<hydra.core.FloatType, java.util.List<hydra.core.FloatType>>) (t -> (t).accept(new hydra.core.FloatType.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.FloatType> visit(hydra.core.FloatType.Bigfloat ignored) {
        return java.util.List.of(
          new hydra.core.FloatType.Float64(),
          new hydra.core.FloatType.Float32());
      }
      
      @Override
      public java.util.List<hydra.core.FloatType> visit(hydra.core.FloatType.Float32 ignored) {
        return java.util.List.of(
          new hydra.core.FloatType.Float64(),
          new hydra.core.FloatType.Bigfloat());
      }
      
      @Override
      public java.util.List<hydra.core.FloatType> visit(hydra.core.FloatType.Float64 ignored) {
        return java.util.List.of(
          new hydra.core.FloatType.Bigfloat(),
          new hydra.core.FloatType.Float32());
      }
    }));
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.coders.AdapterContext>getState(),
      (java.util.function.Function<hydra.coders.AdapterContext, hydra.compute.Flow<hydra.coders.AdapterContext, hydra.compute.Adapter<T0, T1, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>>>) (cx -> {
        java.util.function.Function<hydra.core.FloatType, Boolean> supported = (java.util.function.Function<hydra.core.FloatType, Boolean>) (v1 -> hydra.adapt.utils.Utils.floatTypeIsSupported(
          ((cx).language).constraints,
          v1));
        return hydra.adapt.utils.Utils.chooseAdapter(
          (java.util.function.Function<hydra.core.FloatType, hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T0, T1, hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>>>>) (v1 -> hydra.adapt.literals.Literals.floatAdapter_alts(
            (java.util.function.Function<hydra.util.Precision, java.util.function.Function<hydra.util.Precision, hydra.util.Comparison>>) (p0 -> p1 -> hydra.adapt.literals.Literals.comparePrecision(
              p0,
              p1)),
            (java.util.function.Function<hydra.core.FloatType, java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>>) (p0 -> p1 -> hydra.adapt.literals.Literals.convertFloatValue(
              p0,
              p1)),
            (java.util.function.Function<Boolean, java.util.function.Function<String, java.util.function.Function<String, String>>>) (p0 -> p1 -> p2 -> hydra.adapt.literals.Literals.disclaimer(
              p0,
              p1,
              p2)),
            hydra.reflect.Reflect::floatTypePrecision,
            hydra.show.core.Core::floatType,
            altTypes,
            v1)),
          supported,
          hydra.show.core.Core::floatType,
          hydra.show.core.Core::floatType,
          ft);
      }));
  }
  
  static <T0, T1, T2, T3, T4, T5, T6> hydra.compute.Flow<T4, hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>> floatAdapter_makeAdapter(java.util.function.Function<T0, java.util.function.Function<T0, hydra.util.Comparison>> hydra_adapt_literals_comparePrecision2, java.util.function.Function<T1, java.util.function.Function<T2, T2>> hydra_adapt_literals_convertFloatValue2, java.util.function.Function<Boolean, java.util.function.Function<T3, java.util.function.Function<T3, String>>> hydra_adapt_literals_disclaimer2, java.util.function.Function<T1, T0> hydra_reflect_floatTypePrecision2, java.util.function.Function<T1, T3> hydra_show_core_floatType2, T1 source, T1 target) {
    hydra.util.Lazy<Boolean> lossy = new hydra.util.Lazy<>(() -> hydra.lib.equality.Equal.apply(
      ((hydra_adapt_literals_comparePrecision2).apply((hydra_reflect_floatTypePrecision2).apply(source))).apply((hydra_reflect_floatTypePrecision2).apply(target)),
      new hydra.util.Comparison.GreaterThan()));
    String msg = (((hydra_adapt_literals_disclaimer2).apply(lossy.get())).apply((hydra_show_core_floatType2).apply(source))).apply((hydra_show_core_floatType2).apply(target));
    return hydra.monads.Monads.warn(
      msg,
      hydra.lib.flows.Pure.apply((hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>) ((hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>) ((hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>) ((hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>) ((hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>) ((hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>) (new hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>(lossy.get(), source, target, hydra.adapt.literals.Literals.<T1, T2, T5, T6>floatAdapter_step(
        hydra_adapt_literals_convertFloatValue2,
        source,
        target))))))))));
  }
  
  static <T0, T1, T2, T3, T4, T5, T6> hydra.compute.Flow<T4, java.util.List<hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>>> floatAdapter_alts(java.util.function.Function<T0, java.util.function.Function<T0, hydra.util.Comparison>> hydra_adapt_literals_comparePrecision2, java.util.function.Function<T1, java.util.function.Function<T2, T2>> hydra_adapt_literals_convertFloatValue2, java.util.function.Function<Boolean, java.util.function.Function<T3, java.util.function.Function<T3, String>>> hydra_adapt_literals_disclaimer2, java.util.function.Function<T1, T0> hydra_reflect_floatTypePrecision2, java.util.function.Function<T1, T3> hydra_show_core_floatType2, java.util.function.Function<T1, java.util.List<T1>> altTypes, T1 t) {
    return hydra.lib.flows.MapList.apply(
      ((java.util.function.Function<T1, java.util.function.Function<T1, hydra.compute.Flow<T4, hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>>>>) (v1 -> (java.util.function.Function<T1, hydra.compute.Flow<T4, hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>>>) (v2 -> hydra.adapt.literals.Literals.<T0, T1, T2, T3, T4, T5, T6>floatAdapter_makeAdapter(
        hydra_adapt_literals_comparePrecision2,
        hydra_adapt_literals_convertFloatValue2,
        hydra_adapt_literals_disclaimer2,
        hydra_reflect_floatTypePrecision2,
        hydra_show_core_floatType2,
        v1,
        v2)))).apply(t),
      (altTypes).apply(t));
  }
  
  static <T0, T1, T2, T3> hydra.compute.Coder<T2, T3, T1, T1> floatAdapter_step(java.util.function.Function<T0, java.util.function.Function<T1, T1>> hydra_adapt_literals_convertFloatValue2, T0 source, T0 target) {
    return (hydra.compute.Coder<T2, T3, T1, T1>) ((hydra.compute.Coder<T2, T3, T1, T1>) ((hydra.compute.Coder<T2, T3, T1, T1>) ((hydra.compute.Coder<T2, T3, T1, T1>) (new hydra.compute.Coder<T2, T3, T1, T1>((java.util.function.Function<T1, hydra.compute.Flow<T2, T1>>) (fv -> hydra.lib.flows.Pure.apply(((hydra_adapt_literals_convertFloatValue2).apply(target)).apply(fv))), (java.util.function.Function<T1, hydra.compute.Flow<T3, T1>>) (fv -> hydra.lib.flows.Pure.apply(((hydra_adapt_literals_convertFloatValue2).apply(source)).apply(fv))))))));
  }
  
  static <T0, T1> hydra.compute.Flow<hydra.coders.AdapterContext, hydra.compute.Adapter<T0, T1, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>> integerAdapter(hydra.core.IntegerType it) {
    hydra.util.Lazy<java.util.List<hydra.core.IntegerType>> signedOrdered = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.IntegerType, Boolean>) (v -> hydra.lib.logic.And.apply(
        hydra.reflect.Reflect.integerTypeIsSigned(v),
        hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
          hydra.reflect.Reflect.integerTypePrecision(v),
          new hydra.util.Precision.Arbitrary())))),
      hydra.reflect.Reflect.integerTypes()));
    hydra.util.Lazy<java.util.List<hydra.core.IntegerType>> unsignedOrdered = new hydra.util.Lazy<>(() -> hydra.lib.lists.Filter.apply(
      (java.util.function.Function<hydra.core.IntegerType, Boolean>) (v -> hydra.lib.logic.And.apply(
        hydra.lib.logic.Not.apply(hydra.reflect.Reflect.integerTypeIsSigned(v)),
        hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
          hydra.reflect.Reflect.integerTypePrecision(v),
          new hydra.util.Precision.Arbitrary())))),
      hydra.reflect.Reflect.integerTypes()));
    hydra.util.Lazy<java.util.List<hydra.core.IntegerType>> unsignedPref = new hydra.util.Lazy<>(() -> hydra.adapt.literals.Literals.integerAdapter_interleave(
      unsignedOrdered.get(),
      signedOrdered.get()));
    hydra.util.Lazy<java.util.List<hydra.core.IntegerType>> signedNonPref = new hydra.util.Lazy<>(() -> hydra.lib.lists.Reverse.apply(unsignedPref.get()));
    hydra.util.Lazy<java.util.List<hydra.core.IntegerType>> signedPref = new hydra.util.Lazy<>(() -> hydra.adapt.literals.Literals.integerAdapter_interleave(
      signedOrdered.get(),
      unsignedOrdered.get()));
    java.util.function.Function<Integer, java.util.List<hydra.core.IntegerType>> signed = (java.util.function.Function<Integer, java.util.List<hydra.core.IntegerType>>) (i -> hydra.lib.lists.Concat.apply(java.util.List.of(
      hydra.lib.lists.Drop.apply(
        hydra.lib.math.Mul.apply(
          i,
          2),
        signedPref.get()),
      java.util.List.of(new hydra.core.IntegerType.Bigint()),
      hydra.lib.lists.Drop.apply(
        hydra.lib.math.Add.apply(
          hydra.lib.math.Sub.apply(
            8,
            hydra.lib.math.Mul.apply(
              i,
              2)),
          1),
        signedNonPref.get()))));
    hydra.util.Lazy<java.util.List<hydra.core.IntegerType>> unsignedNonPref = new hydra.util.Lazy<>(() -> hydra.lib.lists.Reverse.apply(signedPref.get()));
    java.util.function.Function<Integer, java.util.List<hydra.core.IntegerType>> unsigned = (java.util.function.Function<Integer, java.util.List<hydra.core.IntegerType>>) (i -> hydra.lib.lists.Concat.apply(java.util.List.of(
      hydra.lib.lists.Drop.apply(
        hydra.lib.math.Mul.apply(
          i,
          2),
        unsignedPref.get()),
      java.util.List.of(new hydra.core.IntegerType.Bigint()),
      hydra.lib.lists.Drop.apply(
        hydra.lib.math.Add.apply(
          hydra.lib.math.Sub.apply(
            8,
            hydra.lib.math.Mul.apply(
              i,
              2)),
          1),
        unsignedNonPref.get()))));
    java.util.function.Function<hydra.core.IntegerType, java.util.List<hydra.core.IntegerType>> altTypes = (java.util.function.Function<hydra.core.IntegerType, java.util.List<hydra.core.IntegerType>>) (t -> (t).accept(new hydra.core.IntegerType.PartialVisitor<>() {
      @Override
      public java.util.List<hydra.core.IntegerType> visit(hydra.core.IntegerType.Bigint ignored) {
        return hydra.lib.lists.Reverse.apply(unsignedPref.get());
      }
      
      @Override
      public java.util.List<hydra.core.IntegerType> visit(hydra.core.IntegerType.Int8 ignored) {
        return (signed).apply(1);
      }
      
      @Override
      public java.util.List<hydra.core.IntegerType> visit(hydra.core.IntegerType.Int16 ignored) {
        return (signed).apply(2);
      }
      
      @Override
      public java.util.List<hydra.core.IntegerType> visit(hydra.core.IntegerType.Int32 ignored) {
        return (signed).apply(3);
      }
      
      @Override
      public java.util.List<hydra.core.IntegerType> visit(hydra.core.IntegerType.Int64 ignored) {
        return (signed).apply(4);
      }
      
      @Override
      public java.util.List<hydra.core.IntegerType> visit(hydra.core.IntegerType.Uint8 ignored) {
        return (unsigned).apply(1);
      }
      
      @Override
      public java.util.List<hydra.core.IntegerType> visit(hydra.core.IntegerType.Uint16 ignored) {
        return (unsigned).apply(2);
      }
      
      @Override
      public java.util.List<hydra.core.IntegerType> visit(hydra.core.IntegerType.Uint32 ignored) {
        return (unsigned).apply(3);
      }
      
      @Override
      public java.util.List<hydra.core.IntegerType> visit(hydra.core.IntegerType.Uint64 ignored) {
        return (unsigned).apply(4);
      }
    }));
    return hydra.lib.flows.Bind.apply(
      hydra.monads.Monads.<hydra.coders.AdapterContext>getState(),
      (java.util.function.Function<hydra.coders.AdapterContext, hydra.compute.Flow<hydra.coders.AdapterContext, hydra.compute.Adapter<T0, T1, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>>>) (cx -> {
        java.util.function.Function<hydra.core.IntegerType, Boolean> supported = (java.util.function.Function<hydra.core.IntegerType, Boolean>) (v1 -> hydra.adapt.utils.Utils.integerTypeIsSupported(
          ((cx).language).constraints,
          v1));
        return hydra.adapt.utils.Utils.chooseAdapter(
          (java.util.function.Function<hydra.core.IntegerType, hydra.compute.Flow<hydra.coders.AdapterContext, java.util.List<hydra.compute.Adapter<T0, T1, hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>>>>) (v1 -> hydra.adapt.literals.Literals.integerAdapter_alts(
            (java.util.function.Function<hydra.util.Precision, java.util.function.Function<hydra.util.Precision, hydra.util.Comparison>>) (p0 -> p1 -> hydra.adapt.literals.Literals.comparePrecision(
              p0,
              p1)),
            (java.util.function.Function<hydra.core.IntegerType, java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>>) (p0 -> p1 -> hydra.adapt.literals.Literals.convertIntegerValue(
              p0,
              p1)),
            (java.util.function.Function<Boolean, java.util.function.Function<String, java.util.function.Function<String, String>>>) (p0 -> p1 -> p2 -> hydra.adapt.literals.Literals.disclaimer(
              p0,
              p1,
              p2)),
            hydra.reflect.Reflect::integerTypePrecision,
            hydra.show.core.Core::integerType,
            altTypes,
            v1)),
          supported,
          hydra.show.core.Core::integerType,
          hydra.show.core.Core::integerType,
          it);
      }));
  }
  
  static <T0> java.util.List<T0> integerAdapter_interleave(java.util.List<T0> xs, java.util.List<T0> ys) {
    return hydra.lib.lists.Concat.apply(hydra.lib.lists.Transpose.apply(java.util.List.of(
      xs,
      ys)));
  }
  
  static <T0, T1, T2, T3, T4, T5, T6> hydra.compute.Flow<T4, hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>> integerAdapter_makeAdapter(java.util.function.Function<T0, java.util.function.Function<T0, hydra.util.Comparison>> hydra_adapt_literals_comparePrecision2, java.util.function.Function<T1, java.util.function.Function<T2, T2>> hydra_adapt_literals_convertIntegerValue2, java.util.function.Function<Boolean, java.util.function.Function<T3, java.util.function.Function<T3, String>>> hydra_adapt_literals_disclaimer2, java.util.function.Function<T1, T0> hydra_reflect_integerTypePrecision2, java.util.function.Function<T1, T3> hydra_show_core_integerType2, T1 source, T1 target) {
    hydra.util.Lazy<Boolean> lossy = new hydra.util.Lazy<>(() -> hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
      ((hydra_adapt_literals_comparePrecision2).apply((hydra_reflect_integerTypePrecision2).apply(source))).apply((hydra_reflect_integerTypePrecision2).apply(target)),
      new hydra.util.Comparison.LessThan())));
    String msg = (((hydra_adapt_literals_disclaimer2).apply(lossy.get())).apply((hydra_show_core_integerType2).apply(source))).apply((hydra_show_core_integerType2).apply(target));
    return hydra.monads.Monads.warn(
      msg,
      hydra.lib.flows.Pure.apply((hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>) ((hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>) ((hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>) ((hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>) ((hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>) ((hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>) (new hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>(lossy.get(), source, target, hydra.adapt.literals.Literals.<T1, T2, T5, T6>integerAdapter_step(
        hydra_adapt_literals_convertIntegerValue2,
        source,
        target))))))))));
  }
  
  static <T0, T1, T2, T3, T4, T5, T6> hydra.compute.Flow<T4, java.util.List<hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>>> integerAdapter_alts(java.util.function.Function<T0, java.util.function.Function<T0, hydra.util.Comparison>> hydra_adapt_literals_comparePrecision2, java.util.function.Function<T1, java.util.function.Function<T2, T2>> hydra_adapt_literals_convertIntegerValue2, java.util.function.Function<Boolean, java.util.function.Function<T3, java.util.function.Function<T3, String>>> hydra_adapt_literals_disclaimer2, java.util.function.Function<T1, T0> hydra_reflect_integerTypePrecision2, java.util.function.Function<T1, T3> hydra_show_core_integerType2, java.util.function.Function<T1, java.util.List<T1>> altTypes, T1 t) {
    return hydra.lib.flows.MapList.apply(
      ((java.util.function.Function<T1, java.util.function.Function<T1, hydra.compute.Flow<T4, hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>>>>) (v1 -> (java.util.function.Function<T1, hydra.compute.Flow<T4, hydra.compute.Adapter<T5, T6, T1, T1, T2, T2>>>) (v2 -> hydra.adapt.literals.Literals.<T0, T1, T2, T3, T4, T5, T6>integerAdapter_makeAdapter(
        hydra_adapt_literals_comparePrecision2,
        hydra_adapt_literals_convertIntegerValue2,
        hydra_adapt_literals_disclaimer2,
        hydra_reflect_integerTypePrecision2,
        hydra_show_core_integerType2,
        v1,
        v2)))).apply(t),
      (altTypes).apply(t));
  }
  
  static <T0, T1, T2, T3> hydra.compute.Coder<T2, T3, T1, T1> integerAdapter_step(java.util.function.Function<T0, java.util.function.Function<T1, T1>> hydra_adapt_literals_convertIntegerValue2, T0 source, T0 target) {
    return (hydra.compute.Coder<T2, T3, T1, T1>) ((hydra.compute.Coder<T2, T3, T1, T1>) ((hydra.compute.Coder<T2, T3, T1, T1>) ((hydra.compute.Coder<T2, T3, T1, T1>) (new hydra.compute.Coder<T2, T3, T1, T1>((java.util.function.Function<T1, hydra.compute.Flow<T2, T1>>) (iv -> hydra.lib.flows.Pure.apply(((hydra_adapt_literals_convertIntegerValue2).apply(target)).apply(iv))), (java.util.function.Function<T1, hydra.compute.Flow<T3, T1>>) (iv -> hydra.lib.flows.Pure.apply(((hydra_adapt_literals_convertIntegerValue2).apply(source)).apply(iv))))))));
  }
}
