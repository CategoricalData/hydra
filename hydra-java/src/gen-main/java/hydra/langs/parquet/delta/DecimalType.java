// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.delta;

import java.io.Serializable;

public class DecimalType implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/delta.DecimalType");
  
  public final Integer precision;
  
  public final Integer scale;
  
  public DecimalType (Integer precision, Integer scale) {
    if (precision == null) {
      throw new IllegalArgumentException("null value for 'precision' argument");
    }
    if (scale == null) {
      throw new IllegalArgumentException("null value for 'scale' argument");
    }
    this.precision = precision;
    this.scale = scale;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DecimalType)) {
      return false;
    }
    DecimalType o = (DecimalType) (other);
    return precision.equals(o.precision) && scale.equals(o.scale);
  }
  
  @Override
  public int hashCode() {
    return 2 * precision.hashCode() + 3 * scale.hashCode();
  }
  
  public DecimalType withPrecision(Integer precision) {
    if (precision == null) {
      throw new IllegalArgumentException("null value for 'precision' argument");
    }
    return new DecimalType(precision, scale);
  }
  
  public DecimalType withScale(Integer scale) {
    if (scale == null) {
      throw new IllegalArgumentException("null value for 'scale' argument");
    }
    return new DecimalType(precision, scale);
  }
}