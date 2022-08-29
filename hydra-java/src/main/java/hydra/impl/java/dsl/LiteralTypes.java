package hydra.impl.java.dsl;

import hydra.core.FloatType;
import hydra.core.IntegerType;
import hydra.core.LiteralType;


public interface LiteralTypes {
  static LiteralType bigfloat() {
    return float_(new FloatType.Bigfloat());
  }

  static LiteralType bigint() {
    return integer(new IntegerType.Bigint());
  }

  static LiteralType binary() {
    return new LiteralType.Binary();
  }

  static LiteralType boolean_() {
    return new LiteralType.Boolean_();
  }

  static LiteralType float32() {
    return float_(new FloatType.Float32());
  }

  static LiteralType float64() {
    return float_(new FloatType.Float64());
  }

  static LiteralType float_(final FloatType ftype) {
    return new LiteralType.Float_(ftype);
  }

  static LiteralType int16() {
    return integer(new IntegerType.Int16());
  }

  static LiteralType int32() {
    return integer(new IntegerType.Int32());
  }

  static LiteralType int64() {
    return integer(new IntegerType.Int64());
  }

  static LiteralType int8() {
    return integer(new IntegerType.Int8());
  }

  static LiteralType integer(final IntegerType itype) {
    return new LiteralType.Integer_(itype);
  }

  static LiteralType uint16() {
    return integer(new IntegerType.Uint16());
  }

  static LiteralType uint32() {
    return integer(new IntegerType.Uint32());
  }

  static LiteralType uint64() {
    return integer(new IntegerType.Uint64());
  }

  static LiteralType uint8() {
    return integer(new IntegerType.Uint8());
  }
}
