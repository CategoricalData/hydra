// Note: this is an automatically generated file. Do not edit.

package hydra.ext.parquet.format;

import java.io.Serializable;

/**
 * Timestamp logical type annotation. Allowed for physical types: INT64
 */
public class TimestampType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/parquet/format.TimestampType");
  
  public static final hydra.core.Name FIELD_NAME_IS_ADJUSTED_TO_UTC = new hydra.core.Name("isAdjustedToUtc");
  
  public static final hydra.core.Name FIELD_NAME_UNIT = new hydra.core.Name("unit");
  
  public final Boolean isAdjustedToUtc;
  
  public final hydra.ext.parquet.format.TimeUnit unit;
  
  public TimestampType (Boolean isAdjustedToUtc, hydra.ext.parquet.format.TimeUnit unit) {
    java.util.Objects.requireNonNull((isAdjustedToUtc));
    java.util.Objects.requireNonNull((unit));
    this.isAdjustedToUtc = isAdjustedToUtc;
    this.unit = unit;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TimestampType)) {
      return false;
    }
    TimestampType o = (TimestampType) (other);
    return isAdjustedToUtc.equals(o.isAdjustedToUtc) && unit.equals(o.unit);
  }
  
  @Override
  public int hashCode() {
    return 2 * isAdjustedToUtc.hashCode() + 3 * unit.hashCode();
  }
  
  public TimestampType withIsAdjustedToUtc(Boolean isAdjustedToUtc) {
    java.util.Objects.requireNonNull((isAdjustedToUtc));
    return new TimestampType(isAdjustedToUtc, unit);
  }
  
  public TimestampType withUnit(hydra.ext.parquet.format.TimeUnit unit) {
    java.util.Objects.requireNonNull((unit));
    return new TimestampType(isAdjustedToUtc, unit);
  }
}
