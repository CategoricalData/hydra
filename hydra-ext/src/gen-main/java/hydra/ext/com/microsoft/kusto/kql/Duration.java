// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.kusto.kql;

import java.io.Serializable;

public class Duration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.com.microsoft.kusto.kql.Duration");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public static final hydra.core.Name FIELD_NAME_UNIT = new hydra.core.Name("unit");
  
  public final Integer value;
  
  public final hydra.ext.com.microsoft.kusto.kql.DurationUnit unit;
  
  public Duration (Integer value, hydra.ext.com.microsoft.kusto.kql.DurationUnit unit) {
    java.util.Objects.requireNonNull((value));
    java.util.Objects.requireNonNull((unit));
    this.value = value;
    this.unit = unit;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Duration)) {
      return false;
    }
    Duration o = (Duration) (other);
    return value.equals(o.value) && unit.equals(o.unit);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode() + 3 * unit.hashCode();
  }
  
  public Duration withValue(Integer value) {
    java.util.Objects.requireNonNull((value));
    return new Duration(value, unit);
  }
  
  public Duration withUnit(hydra.ext.com.microsoft.kusto.kql.DurationUnit unit) {
    java.util.Objects.requireNonNull((unit));
    return new Duration(value, unit);
  }
}