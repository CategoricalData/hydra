// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.format;

import java.io.Serializable;

/**
 * Integer logical type annotation. bitWidth must be 8, 16, 32, or 64. Allowed for physical types: INT32, INT64
 */
public class IntType implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/format.IntType");
  
  public final Byte bitWidth;
  
  public final Boolean isSigned;
  
  public IntType (Byte bitWidth, Boolean isSigned) {
    if (bitWidth == null) {
      throw new IllegalArgumentException("null value for 'bitWidth' argument");
    }
    if (isSigned == null) {
      throw new IllegalArgumentException("null value for 'isSigned' argument");
    }
    this.bitWidth = bitWidth;
    this.isSigned = isSigned;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IntType)) {
      return false;
    }
    IntType o = (IntType) (other);
    return bitWidth.equals(o.bitWidth) && isSigned.equals(o.isSigned);
  }
  
  @Override
  public int hashCode() {
    return 2 * bitWidth.hashCode() + 3 * isSigned.hashCode();
  }
  
  public IntType withBitWidth(Byte bitWidth) {
    if (bitWidth == null) {
      throw new IllegalArgumentException("null value for 'bitWidth' argument");
    }
    return new IntType(bitWidth, isSigned);
  }
  
  public IntType withIsSigned(Boolean isSigned) {
    if (isSigned == null) {
      throw new IllegalArgumentException("null value for 'isSigned' argument");
    }
    return new IntType(bitWidth, isSigned);
  }
}