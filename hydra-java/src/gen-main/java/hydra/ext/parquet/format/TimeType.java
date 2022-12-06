package hydra.ext.parquet.format;

/**
 * Time logical type annotation. Allowed for physical types: INT32 (millis), INT64 (micros, nanos)
 */
public class TimeType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/parquet/format.TimeType");
  
  public final Boolean isAdjustedToUtc;
  
  public final hydra.ext.parquet.format.TimeUnit unit;
  
  public TimeType (Boolean isAdjustedToUtc, hydra.ext.parquet.format.TimeUnit unit) {
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
    return new TimeType(isAdjustedToUtc, unit);
  }
  
  public TimeType withUnit(hydra.ext.parquet.format.TimeUnit unit) {
    return new TimeType(isAdjustedToUtc, unit);
  }
}