// Note: this is an automatically generated file. Do not edit.

package hydra.literals;

/**
 * Conversion functions for literal values.
 */
public interface Literals {
  static hydra.core.FloatValue bigfloatToFloatValue(hydra.core.FloatType ft, java.math.BigDecimal bf) {
    return ((ft)).accept(new hydra.core.FloatType.Visitor<>() {
      @Override
      public hydra.core.FloatValue visit(hydra.core.FloatType.Bigfloat ignored) {
        return new hydra.core.FloatValue.Bigfloat((bf));
      }
      
      @Override
      public hydra.core.FloatValue visit(hydra.core.FloatType.Float32 ignored) {
        return new hydra.core.FloatValue.Float32(hydra.lib.literals.BigfloatToFloat32.apply((bf)));
      }
      
      @Override
      public hydra.core.FloatValue visit(hydra.core.FloatType.Float64 ignored) {
        return new hydra.core.FloatValue.Float64(hydra.lib.literals.BigfloatToFloat64.apply((bf)));
      }
    });
  }
  
  static hydra.core.IntegerValue bigintToIntegerValue(hydra.core.IntegerType it, java.math.BigInteger bi) {
    return ((it)).accept(new hydra.core.IntegerType.Visitor<>() {
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Bigint ignored) {
        return new hydra.core.IntegerValue.Bigint((bi));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Int8 ignored) {
        return new hydra.core.IntegerValue.Int8(hydra.lib.literals.BigintToInt8.apply((bi)));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Int16 ignored) {
        return new hydra.core.IntegerValue.Int16(hydra.lib.literals.BigintToInt16.apply((bi)));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Int32 ignored) {
        return new hydra.core.IntegerValue.Int32(hydra.lib.literals.BigintToInt32.apply((bi)));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Int64 ignored) {
        return new hydra.core.IntegerValue.Int64(hydra.lib.literals.BigintToInt64.apply((bi)));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Uint8 ignored) {
        return new hydra.core.IntegerValue.Uint8(hydra.lib.literals.BigintToUint8.apply((bi)));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Uint16 ignored) {
        return new hydra.core.IntegerValue.Uint16(hydra.lib.literals.BigintToUint16.apply((bi)));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Uint32 ignored) {
        return new hydra.core.IntegerValue.Uint32(hydra.lib.literals.BigintToUint32.apply((bi)));
      }
      
      @Override
      public hydra.core.IntegerValue visit(hydra.core.IntegerType.Uint64 ignored) {
        return new hydra.core.IntegerValue.Uint64(hydra.lib.literals.BigintToUint64.apply((bi)));
      }
    });
  }
  
  static java.math.BigDecimal floatValueToBigfloat(hydra.core.FloatValue v1) {
    return ((v1)).accept(new hydra.core.FloatValue.Visitor<>() {
      @Override
      public java.math.BigDecimal visit(hydra.core.FloatValue.Bigfloat bf) {
        return ((bf)).value;
      }
      
      @Override
      public java.math.BigDecimal visit(hydra.core.FloatValue.Float32 f32) {
        return hydra.lib.literals.Float32ToBigfloat.apply(((f32)).value);
      }
      
      @Override
      public java.math.BigDecimal visit(hydra.core.FloatValue.Float64 f64) {
        return hydra.lib.literals.Float64ToBigfloat.apply(((f64)).value);
      }
    });
  }
  
  static java.math.BigInteger integerValueToBigint(hydra.core.IntegerValue v1) {
    return ((v1)).accept(new hydra.core.IntegerValue.Visitor<>() {
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Bigint bi) {
        return ((bi)).value;
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Int8 i8) {
        return hydra.lib.literals.Int8ToBigint.apply(((i8)).value);
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Int16 i16) {
        return hydra.lib.literals.Int16ToBigint.apply(((i16)).value);
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Int32 i32) {
        return hydra.lib.literals.Int32ToBigint.apply(((i32)).value);
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Int64 i64) {
        return hydra.lib.literals.Int64ToBigint.apply(((i64)).value);
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Uint8 ui8) {
        return hydra.lib.literals.Uint8ToBigint.apply(((ui8)).value);
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Uint16 ui16) {
        return hydra.lib.literals.Uint16ToBigint.apply(((ui16)).value);
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Uint32 ui32) {
        return hydra.lib.literals.Uint32ToBigint.apply(((ui32)).value);
      }
      
      @Override
      public java.math.BigInteger visit(hydra.core.IntegerValue.Uint64 ui64) {
        return hydra.lib.literals.Uint64ToBigint.apply(((ui64)).value);
      }
    });
  }
}
