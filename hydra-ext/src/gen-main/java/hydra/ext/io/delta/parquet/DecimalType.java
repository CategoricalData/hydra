// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.delta.parquet;

import java.io.Serializable;

/**
 * A decimal data type with fixed precision (the maximum number of digits) and scale (the number of digits on right side of dot). The precision can be up to 38, scale can also be up to 38 (less or equal to precision).
 */
public class DecimalType implements Serializable, Comparable<DecimalType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.delta.parquet.DecimalType");
  
  public static final hydra.core.Name PRECISION = new hydra.core.Name("precision");
  
  public static final hydra.core.Name SCALE = new hydra.core.Name("scale");
  
  public final Integer precision;
  
  public final Integer scale;
  
  public DecimalType (Integer precision, Integer scale) {
    this.precision = precision;
    this.scale = scale;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DecimalType)) {
      return false;
    }
    DecimalType o = (DecimalType) other;
    return java.util.Objects.equals(
      this.precision,
      o.precision) && java.util.Objects.equals(
      this.scale,
      o.scale);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(precision) + 3 * java.util.Objects.hashCode(scale);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DecimalType other) {
    int cmp = 0;
    cmp = ((Comparable) precision).compareTo(other.precision);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) scale).compareTo(other.scale);
  }
  
  public DecimalType withPrecision(Integer precision) {
    return new DecimalType(precision, scale);
  }
  
  public DecimalType withScale(Integer scale) {
    return new DecimalType(precision, scale);
  }
}
