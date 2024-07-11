// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.format;

import java.io.Serializable;

/**
 * Time logical type annotation. Allowed for physical types: INT32 (millis), INT64 (micros, nanos)
 */
public class TimeType implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/format.TimeType");
  
  public final Boolean isAdjustedToUtc;
  
  public final hydra.langs.parquet.format.TimeUnit unit;
  
  public TimeType (Boolean isAdjustedToUtc, hydra.langs.parquet.format.TimeUnit unit) {
    java.util.Objects.requireNonNull((isAdjustedToUtc));
    java.util.Objects.requireNonNull((unit));
    this.isAdjustedToUtc = isAdjustedToUtc;
    this.unit = unit;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TimeType)) {
      return false;
    }
    TimeType o = (TimeType) (other);
    return isAdjustedToUtc.equals(o.isAdjustedToUtc) && unit.equals(o.unit);
  }
  
  @Override
  public int hashCode() {
    return 2 * isAdjustedToUtc.hashCode() + 3 * unit.hashCode();
  }
  
  public TimeType withIsAdjustedToUtc(Boolean isAdjustedToUtc) {
    java.util.Objects.requireNonNull((isAdjustedToUtc));
    return new TimeType(isAdjustedToUtc, unit);
  }
  
  public TimeType withUnit(hydra.langs.parquet.format.TimeUnit unit) {
    java.util.Objects.requireNonNull((unit));
    return new TimeType(isAdjustedToUtc, unit);
  }
}