// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.parquet.format;

import java.io.Serializable;

/**
 * Decimal logical type annotation. To maintain forward-compatibility in v1, implementations using this logical type must also set scale and precision on the annotated SchemaElement. Allowed for physical types: INT32, INT64, FIXED, and BINARY
 */
public class DecimalType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.parquet.format.DecimalType");
  
  public static final hydra.core.Name FIELD_NAME_SCALE = new hydra.core.Name("scale");
  
  public static final hydra.core.Name FIELD_NAME_PRECISION = new hydra.core.Name("precision");
  
  public final Integer scale;
  
  public final Integer precision;
  
  public DecimalType (Integer scale, Integer precision) {
    java.util.Objects.requireNonNull((scale));
    java.util.Objects.requireNonNull((precision));
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
    java.util.Objects.requireNonNull((scale));
    return new DecimalType(scale, precision);
  }
  
  public DecimalType withPrecision(Integer precision) {
    java.util.Objects.requireNonNull((precision));
    return new DecimalType(scale, precision);
  }
}