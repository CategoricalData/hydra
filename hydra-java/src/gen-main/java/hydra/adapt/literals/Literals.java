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
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>> literalAdapter(hydra.coders.AdapterContext cx, hydra.core.LiteralType lt) {
    java.util.function.Function<hydra.core.LiteralType, hydra.util.Either<String, java.util.List<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>> alts = (java.util.function.Function<hydra.core.LiteralType, hydra.util.Either<String, java.util.List<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>>) (t -> (t).accept(new hydra.core.LiteralType.PartialVisitor<>() {
      @Override
      public hydra.util.Either<String, java.util.List<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> visit(hydra.core.LiteralType.Binary ignored) {
        return hydra.adapt.literals.Literals.literalAdapter_forBinary(t);
      }
      
      @Override
      public hydra.util.Either<String, java.util.List<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> visit(hydra.core.LiteralType.Boolean_ ignored) {
        return hydra.adapt.literals.Literals.literalAdapter_forBoolean(
          cx,
          (java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.IntegerType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>>>>) (p0 -> p1 -> hydra.adapt.literals.Literals.integerAdapter(
            p0,
            p1)),
          (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Boolean>>>) (p0 -> p1 -> hydra.extract.core.Core.booleanLiteral(
            p0,
            p1)),
          (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, String>>>) (p0 -> p1 -> hydra.extract.core.Core.stringLiteral(
            p0,
            p1)),
          t);
      }
      
      @Override
      public hydra.util.Either<String, java.util.List<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> visit(hydra.core.LiteralType.Float_ ft) {
        return hydra.adapt.literals.Literals.literalAdapter_forFloat(
          cx,
          (java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.FloatType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>>>>) (p0 -> p1 -> hydra.adapt.literals.Literals.floatAdapter(
            p0,
            p1)),
          hydra.show.core.Core::literal,
          t,
          (ft).value);
      }
      
      @Override
      public hydra.util.Either<String, java.util.List<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> visit(hydra.core.LiteralType.Integer_ it) {
        return hydra.adapt.literals.Literals.literalAdapter_forInteger(
          cx,
          (java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.IntegerType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>>>>) (p0 -> p1 -> hydra.adapt.literals.Literals.integerAdapter(
            p0,
            p1)),
          hydra.show.core.Core::literal,
          t,
          (it).value);
      }
      
      @Override
      public hydra.util.Either<String, java.util.List<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> visit(hydra.core.LiteralType.String_ ignored) {
        return (hydra.util.Either<String, java.util.List<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>) ((hydra.util.Either<String, java.util.List<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>) (hydra.util.Either.<String, java.util.List<hydra.compute.Adapter<hydra.core.LiteralType, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>left("no substitute for the literal string type")));
      }
    }));
    java.util.function.Function<hydra.core.LiteralType, Boolean> supported = (java.util.function.Function<hydra.core.LiteralType, Boolean>) (v1 -> hydra.adapt.utils.Utils.literalTypeIsSupported(
      ((cx).language).constraints,
      v1));
    return hydra.adapt.utils.Utils.chooseAdapter(
      alts,
      supported,
      hydra.show.core.Core::literalType,
      hydra.show.core.Core::literalType,
      lt);
  }
  
  static <T0, T1> hydra.util.Either<T1, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> literalAdapter_forBinary(T0 t) {
    hydra.util.Lazy<hydra.compute.Coder<hydra.core.Literal, hydra.core.Literal>> step = new hydra.util.Lazy<>(() -> (hydra.compute.Coder<hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Coder<hydra.core.Literal, hydra.core.Literal>) (new hydra.compute.Coder<hydra.core.Literal, hydra.core.Literal>(p0 -> p1 -> hydra.adapt.literals.Literals.<hydra.context.Context, hydra.context.InContext<hydra.error.Error_>>literalAdapter_matchBinary(
      p0,
      p1), p0 -> p1 -> hydra.adapt.literals.Literals.<hydra.context.Context, hydra.context.InContext<hydra.error.Error_>>literalAdapter_matchString(
      p0,
      p1)))));
    return (hydra.util.Either<T1, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>) ((hydra.util.Either<T1, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>) (hydra.util.Either.<T1, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>right(java.util.List.of((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) (new hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>(false, t, new hydra.core.LiteralType.String_(), step.get())))))))));
  }
  
  static <T0> hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> literalAdapter_forBoolean(hydra.coders.AdapterContext cx, java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.IntegerType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>>>> hydra_adapt_literals_integerAdapter2, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Boolean>>> hydra_extract_core_booleanLiteral2, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, String>>> hydra_extract_core_stringLiteral2, T0 t) {
    hydra.coders.LanguageConstraints constraints = ((cx).language).constraints;
    hydra.util.Lazy<Boolean> hasIntegers = new hydra.util.Lazy<>(() -> hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply((constraints).integerTypes)));
    hydra.util.Lazy<Boolean> hasStrings = new hydra.util.Lazy<>(() -> hydra.lib.sets.Member.apply(
      new hydra.variants.LiteralVariant.String_(),
      (constraints).literalVariants));
    java.util.function.Function<hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>> matchBoolean = (java.util.function.Function<hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>>) (step_ -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>) (cx2 -> (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>) (lit -> (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal> visit(hydra.core.Literal.Boolean_ bv) {
        return hydra.lib.eithers.Bind.apply(
          ((((java.util.function.Function<hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>>>>) (projected -> projected.encode))).apply(step_)).apply(cx2)).apply(new hydra.core.IntegerValue.Uint8(hydra.lib.logic.IfElse.lazy(
            (bv).value,
            () -> (short) (1),
            () -> (short) (0)))),
          (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>) (iv -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>right(new hydra.core.Literal.Integer_(iv))))));
      }
    }))));
    java.util.function.Function<hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>> matchInteger = (java.util.function.Function<hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>>) (step_ -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>) (cx2 -> (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>) (lit -> {
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
        public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal> visit(hydra.core.Literal.Integer_ iv) {
          return hydra.lib.eithers.Bind.apply(
            ((((java.util.function.Function<hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>>>>) ((java.util.function.Function<hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>>>>) (projected -> projected.decode))).apply(step_)).apply(cx2)).apply((iv).value),
            (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>) (val -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>right((forValue).apply(val))))));
        }
      });
    })));
    return hydra.lib.logic.IfElse.lazy(
      hasIntegers.get(),
      () -> hydra.adapt.literals.Literals.<T0>literalAdapter_withIntegers2(
        cx,
        hydra_adapt_literals_integerAdapter2,
        matchBoolean,
        matchInteger,
        t),
      () -> hydra.lib.logic.IfElse.lazy(
        hasStrings.get(),
        () -> (hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>) ((hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>) (hydra.util.Either.<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>right(hydra.adapt.literals.Literals.<T0>literalAdapter_withStrings(
          hydra_extract_core_booleanLiteral2,
          hydra_extract_core_stringLiteral2,
          t)))),
        () -> (hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>) ((hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>) (hydra.util.Either.<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>left("no alternatives available for boolean encoding")))));
  }
  
  static <T0> hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> literalAdapter_forFloat(hydra.coders.AdapterContext cx, java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.FloatType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>>>> hydra_adapt_literals_floatAdapter2, java.util.function.Function<hydra.core.Literal, String> hydra_show_core_literal2, T0 t, hydra.core.FloatType ft) {
    hydra.coders.LanguageConstraints constraints = ((cx).language).constraints;
    hydra.util.Lazy<Boolean> hasFloats = new hydra.util.Lazy<>(() -> hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply((constraints).floatTypes)));
    return hydra.lib.logic.IfElse.lazy(
      hasFloats.get(),
      () -> hydra.adapt.literals.Literals.<T0>literalAdapter_withFloats(
        cx,
        ft,
        hydra_adapt_literals_floatAdapter2,
        hydra_show_core_literal2,
        t),
      () -> (hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>) ((hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>) (hydra.util.Either.<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>left("no float types available"))));
  }
  
  static <T0> hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> literalAdapter_forInteger(hydra.coders.AdapterContext cx, java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.IntegerType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>>>> hydra_adapt_literals_integerAdapter2, java.util.function.Function<hydra.core.Literal, String> hydra_show_core_literal2, T0 t, hydra.core.IntegerType it) {
    hydra.coders.LanguageConstraints constraints = ((cx).language).constraints;
    hydra.util.Lazy<Boolean> hasIntegers = new hydra.util.Lazy<>(() -> hydra.lib.logic.Not.apply(hydra.lib.sets.Null.apply((constraints).integerTypes)));
    return hydra.lib.logic.IfElse.lazy(
      hasIntegers.get(),
      () -> hydra.adapt.literals.Literals.<T0>literalAdapter_withIntegers(
        cx,
        hydra_adapt_literals_integerAdapter2,
        hydra_show_core_literal2,
        it,
        t),
      () -> (hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>) ((hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>) (hydra.util.Either.<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>left("no integer types available"))));
  }
  
  static <T0> hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> literalAdapter_withIntegers(hydra.coders.AdapterContext cx, java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.IntegerType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>>>> hydra_adapt_literals_integerAdapter2, java.util.function.Function<hydra.core.Literal, String> hydra_show_core_literal2, hydra.core.IntegerType it, T0 t) {
    return hydra.lib.eithers.Bind.apply(
      ((hydra_adapt_literals_integerAdapter2).apply(cx)).apply(it),
      (java.util.function.Function<hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>>) (adapter -> {
        hydra.util.Lazy<hydra.compute.Coder<hydra.core.Literal, hydra.core.Literal>> step = new hydra.util.Lazy<>(() -> hydra.adapt.utils.Utils.bidirectional((java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>>) (v1 -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>) (v2 -> (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>) (v3 -> hydra.adapt.literals.Literals.literalAdapter_adapt(
          hydra_show_core_literal2,
          adapter,
          v1,
          v2,
          v3))))));
        return (hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>) ((hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>) (hydra.util.Either.<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>right(java.util.List.of((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) (new hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>(((java.util.function.Function<hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, Boolean>) (projected -> projected.isLossy))))).apply(adapter), t, new hydra.core.LiteralType.Integer_(((java.util.function.Function<hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) (projected -> projected.target))))).apply(adapter)), step.get())))))))));
      }));
  }
  
  static <T1, T2> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal> literalAdapter_adapt(java.util.function.Function<hydra.core.Literal, String> hydra_show_core_literal2, hydra.compute.Adapter<T1, T2, hydra.core.IntegerValue, hydra.core.IntegerValue> adapter, hydra.coders.CoderDirection dir, hydra.context.Context cx, hydra.core.Literal lit) {
    return (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal> otherwise(hydra.core.Literal instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          "expected integer literal, found ",
          (hydra_show_core_literal2).apply(lit)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal> visit(hydra.core.Literal.Integer_ iv) {
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.core.IntegerValue, hydra.core.Literal>) (x -> new hydra.core.Literal.Integer_(x)),
          hydra.adapt.utils.Utils.encodeDecode(
            dir,
            ((java.util.function.Function<hydra.compute.Adapter<T1, T2, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>>) ((java.util.function.Function<hydra.compute.Adapter<T1, T2, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>>) ((java.util.function.Function<hydra.compute.Adapter<T1, T2, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>>) ((java.util.function.Function<hydra.compute.Adapter<T1, T2, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>>) (projected -> projected.coder))))).apply(adapter),
            cx,
            (iv).value));
      }
    });
  }
  
  static <T0> hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> literalAdapter_withFloats(hydra.coders.AdapterContext cx, hydra.core.FloatType ft, java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.FloatType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>>>> hydra_adapt_literals_floatAdapter2, java.util.function.Function<hydra.core.Literal, String> hydra_show_core_literal2, T0 t) {
    return hydra.lib.eithers.Bind.apply(
      ((hydra_adapt_literals_floatAdapter2).apply(cx)).apply(ft),
      (java.util.function.Function<hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>>) (adapter -> {
        hydra.util.Lazy<hydra.compute.Coder<hydra.core.Literal, hydra.core.Literal>> step = new hydra.util.Lazy<>(() -> hydra.adapt.utils.Utils.bidirectional((java.util.function.Function<hydra.coders.CoderDirection, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>>) (v1 -> (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>) (v2 -> (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>) (v3 -> hydra.adapt.literals.Literals.literalAdapter_adapt2(
          hydra_show_core_literal2,
          adapter,
          v1,
          v2,
          v3))))));
        return (hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>) ((hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>) (hydra.util.Either.<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>right(java.util.List.of((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) (new hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>(((java.util.function.Function<hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, Boolean>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, Boolean>) (projected -> projected.isLossy))))).apply(adapter), t, new hydra.core.LiteralType.Float_(((java.util.function.Function<hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.core.FloatType>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.core.FloatType>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.core.FloatType>) ((java.util.function.Function<hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.core.FloatType>) (projected -> projected.target))))).apply(adapter)), step.get())))))))));
      }));
  }
  
  static <T1, T2> hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal> literalAdapter_adapt2(java.util.function.Function<hydra.core.Literal, String> hydra_show_core_literal2, hydra.compute.Adapter<T1, T2, hydra.core.FloatValue, hydra.core.FloatValue> adapter, hydra.coders.CoderDirection dir, hydra.context.Context cx, hydra.core.Literal l) {
    return (l).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal> otherwise(hydra.core.Literal instance) {
        return (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
          "expected floating-point literal, found ",
          (hydra_show_core_literal2).apply(l)))), cx)))));
      }
      
      @Override
      public hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal> visit(hydra.core.Literal.Float_ fv) {
        return hydra.lib.eithers.Map.apply(
          (java.util.function.Function<hydra.core.FloatValue, hydra.core.Literal>) (x -> new hydra.core.Literal.Float_(x)),
          hydra.adapt.utils.Utils.encodeDecode(
            dir,
            ((java.util.function.Function<hydra.compute.Adapter<T1, T2, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.compute.Coder<hydra.core.FloatValue, hydra.core.FloatValue>>) ((java.util.function.Function<hydra.compute.Adapter<T1, T2, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.compute.Coder<hydra.core.FloatValue, hydra.core.FloatValue>>) ((java.util.function.Function<hydra.compute.Adapter<T1, T2, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.compute.Coder<hydra.core.FloatValue, hydra.core.FloatValue>>) ((java.util.function.Function<hydra.compute.Adapter<T1, T2, hydra.core.FloatValue, hydra.core.FloatValue>, hydra.compute.Coder<hydra.core.FloatValue, hydra.core.FloatValue>>) (projected -> projected.coder))))).apply(adapter),
            cx,
            (fv).value));
      }
    });
  }
  
  static <T0> hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> literalAdapter_withIntegers2(hydra.coders.AdapterContext cx, java.util.function.Function<hydra.coders.AdapterContext, java.util.function.Function<hydra.core.IntegerType, hydra.util.Either<String, hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>>>> hydra_adapt_literals_integerAdapter2, java.util.function.Function<hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>> matchBoolean, java.util.function.Function<hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>> matchInteger, T0 t) {
    return hydra.lib.eithers.Bind.apply(
      ((hydra_adapt_literals_integerAdapter2).apply(cx)).apply(new hydra.core.IntegerType.Uint8()),
      (java.util.function.Function<hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.util.Either<String, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>>) (adapter -> hydra.adapt.literals.Literals.literalAdapter_withAdapter(
        matchBoolean,
        matchInteger,
        t,
        adapter)));
  }
  
  static <T0> java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>> literalAdapter_withStrings(java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, Boolean>>> hydra_extract_core_booleanLiteral2, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, String>>> hydra_extract_core_stringLiteral2, T0 t) {
    java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>> decode = (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>) (cx -> (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>) (lit -> hydra.lib.eithers.Bind.apply(
      ((hydra_extract_core_stringLiteral2).apply(cx)).apply(lit),
      (java.util.function.Function<String, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>) (s -> hydra.lib.logic.IfElse.lazy(
        hydra.lib.equality.Equal.apply(
          s,
          "true"),
        () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>right(new hydra.core.Literal.Boolean_(true)))),
        () -> hydra.lib.logic.IfElse.lazy(
          hydra.lib.equality.Equal.apply(
            s,
            "false"),
          () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>right(new hydra.core.Literal.Boolean_(false)))),
          () -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>left((hydra.context.InContext<hydra.error.Error_>) (new hydra.context.InContext<hydra.error.Error_>(new hydra.error.Error_.Other(new hydra.error.OtherError(hydra.lib.strings.Cat2.apply(
            "expected boolean literal, found ",
            s))), cx)))))))))));
    java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>> encode = (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>) (cx -> (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>) (lit -> hydra.lib.eithers.Bind.apply(
      ((hydra_extract_core_booleanLiteral2).apply(cx)).apply(lit),
      (java.util.function.Function<Boolean, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>) (b -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>right(new hydra.core.Literal.String_(hydra.lib.logic.IfElse.lazy(
        b,
        () -> "true",
        () -> "false")))))))));
    return java.util.List.of((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) (new hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>(false, t, new hydra.core.LiteralType.String_(), (hydra.compute.Coder<hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Coder<hydra.core.Literal, hydra.core.Literal>) (new hydra.compute.Coder<hydra.core.Literal, hydra.core.Literal>(encode, decode)))))))));
  }
  
  static <T0, T1, T2> hydra.util.Either<T2, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>> literalAdapter_withAdapter(java.util.function.Function<hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>> matchBoolean, java.util.function.Function<hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>, java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>> matchInteger, T0 t, hydra.compute.Adapter<T1, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue> adapter) {
    hydra.util.Lazy<hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>> step_ = new hydra.util.Lazy<>(() -> ((java.util.function.Function<hydra.compute.Adapter<T1, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>>) ((java.util.function.Function<hydra.compute.Adapter<T1, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>>) ((java.util.function.Function<hydra.compute.Adapter<T1, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>>) ((java.util.function.Function<hydra.compute.Adapter<T1, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>>) (projected -> projected.coder))))).apply(adapter));
    hydra.util.Lazy<hydra.compute.Coder<hydra.core.Literal, hydra.core.Literal>> step = new hydra.util.Lazy<>(() -> (hydra.compute.Coder<hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Coder<hydra.core.Literal, hydra.core.Literal>) (new hydra.compute.Coder<hydra.core.Literal, hydra.core.Literal>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>) (v1 -> (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>) (v2 -> (((matchBoolean).apply(step_.get())).apply(v1)).apply(v2))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>>) (v1 -> (java.util.function.Function<hydra.core.Literal, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.Literal>>) (v2 -> (((matchInteger).apply(step_.get())).apply(v1)).apply(v2)))))));
    return (hydra.util.Either<T2, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>) ((hydra.util.Either<T2, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>) (hydra.util.Either.<T2, java.util.List<hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>>>right(java.util.List.of((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) ((hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>) (new hydra.compute.Adapter<T0, hydra.core.LiteralType, hydra.core.Literal, hydra.core.Literal>(false, t, new hydra.core.LiteralType.Integer_(((java.util.function.Function<hydra.compute.Adapter<T1, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) ((java.util.function.Function<hydra.compute.Adapter<T1, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) ((java.util.function.Function<hydra.compute.Adapter<T1, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) ((java.util.function.Function<hydra.compute.Adapter<T1, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>, hydra.core.IntegerType>) (projected -> projected.target))))).apply(adapter)), step.get())))))))));
  }
  
  static <T2, T3> hydra.util.Either<T3, hydra.core.Literal> literalAdapter_matchBinary(T2 _cx, hydra.core.Literal lit) {
    return (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T3, hydra.core.Literal> visit(hydra.core.Literal.Binary b) {
        return (hydra.util.Either<T3, hydra.core.Literal>) ((hydra.util.Either<T3, hydra.core.Literal>) (hydra.util.Either.<T3, hydra.core.Literal>right(new hydra.core.Literal.String_(hydra.lib.literals.BinaryToString.apply((b).value)))));
      }
    });
  }
  
  static <T2, T3> hydra.util.Either<T3, hydra.core.Literal> literalAdapter_matchString(T2 _cx, hydra.core.Literal lit) {
    return (lit).accept(new hydra.core.Literal.PartialVisitor<>() {
      @Override
      public hydra.util.Either<T3, hydra.core.Literal> visit(hydra.core.Literal.String_ s) {
        return (hydra.util.Either<T3, hydra.core.Literal>) ((hydra.util.Either<T3, hydra.core.Literal>) (hydra.util.Either.<T3, hydra.core.Literal>right(new hydra.core.Literal.Binary(hydra.lib.literals.StringToBinary.apply((s).value)))));
      }
    });
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>> floatAdapter(hydra.coders.AdapterContext cx, hydra.core.FloatType ft) {
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
    java.util.function.Function<hydra.core.FloatType, Boolean> supported = (java.util.function.Function<hydra.core.FloatType, Boolean>) (v1 -> hydra.adapt.utils.Utils.floatTypeIsSupported(
      ((cx).language).constraints,
      v1));
    return hydra.adapt.utils.Utils.chooseAdapter(
      (java.util.function.Function<hydra.core.FloatType, hydra.util.Either<String, java.util.List<hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>>>>) (v1 -> hydra.adapt.literals.Literals.floatAdapter_alts(
        (java.util.function.Function<hydra.util.Precision, java.util.function.Function<hydra.util.Precision, hydra.util.Comparison>>) (p0 -> p1 -> hydra.adapt.literals.Literals.comparePrecision(
          p0,
          p1)),
        (java.util.function.Function<hydra.core.FloatType, java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>>) (p0 -> p1 -> hydra.adapt.literals.Literals.convertFloatValue(
          p0,
          p1)),
        hydra.reflect.Reflect::floatTypePrecision,
        altTypes,
        v1)),
      supported,
      hydra.show.core.Core::floatType,
      hydra.show.core.Core::floatType,
      ft);
  }
  
  static <T0> hydra.util.Either<T0, hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>> floatAdapter_makeAdapter(java.util.function.Function<hydra.util.Precision, java.util.function.Function<hydra.util.Precision, hydra.util.Comparison>> hydra_adapt_literals_comparePrecision2, java.util.function.Function<hydra.core.FloatType, java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>> hydra_adapt_literals_convertFloatValue2, java.util.function.Function<hydra.core.FloatType, hydra.util.Precision> hydra_reflect_floatTypePrecision2, hydra.core.FloatType source, hydra.core.FloatType target) {
    hydra.util.Lazy<Boolean> lossy = new hydra.util.Lazy<>(() -> hydra.lib.equality.Equal.apply(
      ((hydra_adapt_literals_comparePrecision2).apply((hydra_reflect_floatTypePrecision2).apply(source))).apply((hydra_reflect_floatTypePrecision2).apply(target)),
      new hydra.util.Comparison.GreaterThan()));
    hydra.util.Lazy<hydra.compute.Coder<hydra.core.FloatValue, hydra.core.FloatValue>> step = new hydra.util.Lazy<>(() -> (hydra.compute.Coder<hydra.core.FloatValue, hydra.core.FloatValue>) ((hydra.compute.Coder<hydra.core.FloatValue, hydra.core.FloatValue>) (new hydra.compute.Coder<hydra.core.FloatValue, hydra.core.FloatValue>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.FloatValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue>>>) (_cx -> (java.util.function.Function<hydra.core.FloatValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue>>) (fv -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue>right(((hydra_adapt_literals_convertFloatValue2).apply(target)).apply(fv)))))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.FloatValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue>>>) (_cx -> (java.util.function.Function<hydra.core.FloatValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue>>) (fv -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.FloatValue>right(((hydra_adapt_literals_convertFloatValue2).apply(source)).apply(fv))))))))));
    return (hydra.util.Either<T0, hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>>) ((hydra.util.Either<T0, hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>>) (hydra.util.Either.<T0, hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>>right((hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>) ((hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>) ((hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>) ((hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>) (new hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>(lossy.get(), source, target, step.get()))))))));
  }
  
  static <T0> hydra.util.Either<T0, java.util.List<hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>>> floatAdapter_alts(java.util.function.Function<hydra.util.Precision, java.util.function.Function<hydra.util.Precision, hydra.util.Comparison>> hydra_adapt_literals_comparePrecision2, java.util.function.Function<hydra.core.FloatType, java.util.function.Function<hydra.core.FloatValue, hydra.core.FloatValue>> hydra_adapt_literals_convertFloatValue2, java.util.function.Function<hydra.core.FloatType, hydra.util.Precision> hydra_reflect_floatTypePrecision2, java.util.function.Function<hydra.core.FloatType, java.util.List<hydra.core.FloatType>> altTypes, hydra.core.FloatType t) {
    return hydra.lib.eithers.MapList.apply(
      (java.util.function.Function<hydra.core.FloatType, hydra.util.Either<T0, hydra.compute.Adapter<hydra.core.FloatType, hydra.core.FloatType, hydra.core.FloatValue, hydra.core.FloatValue>>>) (v1 -> hydra.adapt.literals.Literals.<T0>floatAdapter_makeAdapter(
        hydra_adapt_literals_comparePrecision2,
        hydra_adapt_literals_convertFloatValue2,
        hydra_reflect_floatTypePrecision2,
        t,
        v1)),
      (altTypes).apply(t));
  }
  
  static hydra.util.Either<String, hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>> integerAdapter(hydra.coders.AdapterContext cx, hydra.core.IntegerType it) {
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
    java.util.function.Function<hydra.core.IntegerType, Boolean> supported = (java.util.function.Function<hydra.core.IntegerType, Boolean>) (v1 -> hydra.adapt.utils.Utils.integerTypeIsSupported(
      ((cx).language).constraints,
      v1));
    return hydra.adapt.utils.Utils.chooseAdapter(
      (java.util.function.Function<hydra.core.IntegerType, hydra.util.Either<String, java.util.List<hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>>>>) (v1 -> hydra.adapt.literals.Literals.integerAdapter_alts(
        (java.util.function.Function<hydra.util.Precision, java.util.function.Function<hydra.util.Precision, hydra.util.Comparison>>) (p0 -> p1 -> hydra.adapt.literals.Literals.comparePrecision(
          p0,
          p1)),
        (java.util.function.Function<hydra.core.IntegerType, java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>>) (p0 -> p1 -> hydra.adapt.literals.Literals.convertIntegerValue(
          p0,
          p1)),
        hydra.reflect.Reflect::integerTypePrecision,
        altTypes,
        v1)),
      supported,
      hydra.show.core.Core::integerType,
      hydra.show.core.Core::integerType,
      it);
  }
  
  static <T0> java.util.List<T0> integerAdapter_interleave(java.util.List<T0> xs, java.util.List<T0> ys) {
    return hydra.lib.lists.Concat.apply(hydra.lib.lists.Transpose.apply(java.util.List.of(
      xs,
      ys)));
  }
  
  static <T0> hydra.util.Either<T0, hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>> integerAdapter_makeAdapter(java.util.function.Function<hydra.util.Precision, java.util.function.Function<hydra.util.Precision, hydra.util.Comparison>> hydra_adapt_literals_comparePrecision2, java.util.function.Function<hydra.core.IntegerType, java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>> hydra_adapt_literals_convertIntegerValue2, java.util.function.Function<hydra.core.IntegerType, hydra.util.Precision> hydra_reflect_integerTypePrecision2, hydra.core.IntegerType source, hydra.core.IntegerType target) {
    hydra.util.Lazy<Boolean> lossy = new hydra.util.Lazy<>(() -> hydra.lib.logic.Not.apply(hydra.lib.equality.Equal.apply(
      ((hydra_adapt_literals_comparePrecision2).apply((hydra_reflect_integerTypePrecision2).apply(source))).apply((hydra_reflect_integerTypePrecision2).apply(target)),
      new hydra.util.Comparison.LessThan())));
    hydra.util.Lazy<hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>> step = new hydra.util.Lazy<>(() -> (hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>) ((hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>) (new hydra.compute.Coder<hydra.core.IntegerValue, hydra.core.IntegerValue>((java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>>>) (_cx -> (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>>) (iv -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>right(((hydra_adapt_literals_convertIntegerValue2).apply(target)).apply(iv)))))), (java.util.function.Function<hydra.context.Context, java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>>>) (_cx -> (java.util.function.Function<hydra.core.IntegerValue, hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>>) (iv -> (hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>) ((hydra.util.Either<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>) (hydra.util.Either.<hydra.context.InContext<hydra.error.Error_>, hydra.core.IntegerValue>right(((hydra_adapt_literals_convertIntegerValue2).apply(source)).apply(iv))))))))));
    return (hydra.util.Either<T0, hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>>) ((hydra.util.Either<T0, hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>>) (hydra.util.Either.<T0, hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>>right((hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>) ((hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>) ((hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>) ((hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>) (new hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>(lossy.get(), source, target, step.get()))))))));
  }
  
  static <T0> hydra.util.Either<T0, java.util.List<hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>>> integerAdapter_alts(java.util.function.Function<hydra.util.Precision, java.util.function.Function<hydra.util.Precision, hydra.util.Comparison>> hydra_adapt_literals_comparePrecision2, java.util.function.Function<hydra.core.IntegerType, java.util.function.Function<hydra.core.IntegerValue, hydra.core.IntegerValue>> hydra_adapt_literals_convertIntegerValue2, java.util.function.Function<hydra.core.IntegerType, hydra.util.Precision> hydra_reflect_integerTypePrecision2, java.util.function.Function<hydra.core.IntegerType, java.util.List<hydra.core.IntegerType>> altTypes, hydra.core.IntegerType t) {
    return hydra.lib.eithers.MapList.apply(
      (java.util.function.Function<hydra.core.IntegerType, hydra.util.Either<T0, hydra.compute.Adapter<hydra.core.IntegerType, hydra.core.IntegerType, hydra.core.IntegerValue, hydra.core.IntegerValue>>>) (v1 -> hydra.adapt.literals.Literals.<T0>integerAdapter_makeAdapter(
        hydra_adapt_literals_comparePrecision2,
        hydra_adapt_literals_convertIntegerValue2,
        hydra_reflect_integerTypePrecision2,
        t,
        v1)),
      (altTypes).apply(t));
  }
}
