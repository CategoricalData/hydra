// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.format;

import java.io.Serializable;

/**
 * Decimal logical type annotation. To maintain forward-compatibility in v1, implementations using this logical type must also set scale and precision on the annotated SchemaElement. Allowed for physical types: INT32, INT64, FIXED, and BINARY
 */
public class DecimalType implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/format.DecimalType");
  
  public final Integer scale;
  
  public final Integer precision;
  
  public DecimalType (Integer scale, Integer precision) {
    if (scale == null) {
      throw new IllegalArgumentException("null value for 'scale' argument");
    }
    if (precision == null) {
      throw new IllegalArgumentException("null value for 'precision' argument");
    }
    this.scale = scale;
    this.precision = precision;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DecimalType)) {
      return false;
    }
    DecimalType o = (DecimalType) (other);
    return scale.equals(o.scale) && precision.equals(o.precision);
  }
  
  @Override
  public int hashCode() {
    return 2 * scale.hashCode() + 3 * precision.hashCode();
  }
  
  public DecimalType withScale(Integer scale) {
    if (scale == null) {
      throw new IllegalArgumentException("null value for 'scale' argument");
    }
    return new DecimalType(scale, precision);
  }
  
  public DecimalType withPrecision(Integer precision) {
    if (precision == null) {
      throw new IllegalArgumentException("null value for 'precision' argument");
    }
    return new DecimalType(scale, precision);
  }
}