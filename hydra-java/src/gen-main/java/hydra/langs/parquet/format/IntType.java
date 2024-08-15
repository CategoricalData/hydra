// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.format;

import java.io.Serializable;

/**
 * Integer logical type annotation. bitWidth must be 8, 16, 32, or 64. Allowed for physical types: INT32, INT64
 */
public class IntType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/parquet/format.IntType");
  
  public static final hydra.core.Name FIELD_NAME_BIT_WIDTH = new hydra.core.Name("bitWidth");
  
  public static final hydra.core.Name FIELD_NAME_IS_SIGNED = new hydra.core.Name("isSigned");
  
  public final Character bitWidth;
  
  public final Boolean isSigned;
  
  public IntType (Character bitWidth, Boolean isSigned) {
    java.util.Objects.requireNonNull((bitWidth));
    java.util.Objects.requireNonNull((isSigned));
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
  
  public IntType withBitWidth(Character bitWidth) {
    java.util.Objects.requireNonNull((bitWidth));
    return new IntType(bitWidth, isSigned);
  }
  
  public IntType withIsSigned(Boolean isSigned) {
    java.util.Objects.requireNonNull((isSigned));
    return new IntType(bitWidth, isSigned);
  }
}