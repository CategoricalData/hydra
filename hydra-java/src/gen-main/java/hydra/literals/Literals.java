// Note: this is an automatically generated file. Do not edit.

package hydra.literals;

/**
 * Conversion functions for literal values.
 */
public interface Literals {
  static java.util.function.Function<java.math.BigDecimal, hydra.core.FloatValue> bigfloatToFloatValue(hydra.core.FloatType ft) {
    return (java.util.function.Function<java.math.BigDecimal, hydra.core.FloatValue>) (bf -> ((ft)).accept(new hydra.core.FloatType.Visitor<>() {
      @Override
      public hydra.core.FloatValue visit(hydra.core.FloatType.Bigfloat instance) {
        return new hydra.core.FloatValue.Bigfloat((bf));
      }
      
      @Override
      public hydra.core.FloatValue visit(hydra.core.FloatType.Float32 instance) {
        return new hydra.core.FloatValue.Float32(hydra.lib.literals.BigfloatToFloat32.apply((bf)));
      }
      
      @Override
      public hydra.core.FloatValue visit(hydra.core.FloatType.Float64 instance) {
        return new hydra.core.FloatValue.Float64(hydra.lib.literals.BigfloatToFloat64.apply((bf)));
      }
    }));
  }
  
  static java.util.function.Function<java.math.BigInteger, hydra.core.IntegerValue> bigintToIntegerValue(hydra.core.IntegerType it) {
    return (java.util.function.Function<java.math.BigInteger, hydra.core.IntegerValue>) (bi -> ((it)).accept(new hydra.core.IntegerType.Visitor<>() {
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Bigint instance) {
        return new hydra.core.IntegerValue.Bigint((bi));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Int8 instance) {
        return new hydra.core.IntegerValue.Int8(hydra.lib.literals.BigintToInt8.apply((bi)));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Int16 instance) {
        return new hydra.core.IntegerValue.Int16(hydra.lib.literals.BigintToInt16.apply((bi)));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Int32 instance) {
        return new hydra.core.IntegerValue.Int32(hydra.lib.literals.BigintToInt32.apply((bi)));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Int64 instance) {
        return new hydra.core.IntegerValue.Int64(hydra.lib.literals.BigintToInt64.apply((bi)));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Uint8 instance) {
        return new hydra.core.IntegerValue.Uint8(hydra.lib.literals.BigintToUint8.apply((bi)));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Uint16 instance) {
        return new hydra.core.IntegerValue.Uint16(hydra.lib.literals.BigintToUint16.apply((bi)));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Uint32 instance) {
        return new hydra.core.IntegerValue.Uint32(hydra.lib.literals.BigintToUint32.apply((bi)));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Uint64 instance) {
        return new hydra.core.IntegerValue.Uint64(hydra.lib.literals.BigintToUint64.apply((bi)));
      }
    }));
  }
  
  static java.math.BigDecimal floatValueToBigfloat(hydra.core.FloatValue v1) {
    return ((v1)).accept(new hydra.core.FloatValue.Visitor<>() {
      @Override
      public java.math.BigDecimal visit(hydra.core.FloatValue.Bigfloat instance) {
        return (instance.value);
      }
      
      @Override
      public java.math.BigDecimal visit(hydra.core.FloatValue.Float32 instance) {
        return hydra.lib.literals.Float32ToBigfloat.apply((instance.value));
      }
      
      @Override
      public java.math.BigDecimal visit(hydra.core.FloatValue.Float64 instance) {
        return hydra.lib.literals.Float64ToBigfloat.apply((instance.value));
      }
    });
  }
  
  static java.math.BigInteger integerValueToBigint(hydra.core.IntegerValue v1) {
    return ((v1)).accept(new hydra.core.IntegerValue.Visitor<>() {
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Bigint instance) {
        return (instance.value);
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Int8 instance) {
        return hydra.lib.literals.Int8ToBigint.apply((instance.value));
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Int16 instance) {
        return hydra.lib.literals.Int16ToBigint.apply((instance.value));
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Int32 instance) {
        return hydra.lib.literals.Int32ToBigint.apply((instance.value));
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Int64 instance) {
        return hydra.lib.literals.Int64ToBigint.apply((instance.value));
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Uint8 instance) {
        return hydra.lib.literals.Uint8ToBigint.apply((instance.value));
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Uint16 instance) {
        return hydra.lib.literals.Uint16ToBigint.apply((instance.value));
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Uint32 instance) {
        return hydra.lib.literals.Uint32ToBigint.apply((instance.value));
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Uint64 instance) {
        return hydra.lib.literals.Uint64ToBigint.apply((instance.value));
      }
    });
  }
}
