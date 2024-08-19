// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.delta;

import java.io.Serializable;

/**
 * A decimal data type with fixed precision (the maximum number of digits) and scale (the number of digits on right side of dot). The precision can be up to 38, scale can also be up to 38 (less or equal to precision).
 */
public class DecimalType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/parquet/delta.DecimalType");
  
  public static final hydra.core.Name FIELD_NAME_PRECISION = new hydra.core.Name("precision");
  
  public static final hydra.core.Name FIELD_NAME_SCALE = new hydra.core.Name("scale");
  
  public final Integer precision;
  
  public final Integer scale;
  
  public DecimalType (Integer precision, Integer scale) {
    java.util.Objects.requireNonNull((precision));
    java.util.Objects.requireNonNull((scale));
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
    java.util.Objects.requireNonNull((precision));
    return new DecimalType(precision, scale);
  }
  
  public DecimalType withScale(Integer scale) {
    java.util.Objects.requireNonNull((scale));
    return new DecimalType(precision, scale);
  }
}