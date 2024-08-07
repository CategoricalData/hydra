// Note: this is an automatically generated file. Do not edit.

package hydra.langs.kusto.kql;

import java.io.Serializable;

public class Duration implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/kusto/kql.Duration");
  
  public final Integer value;
  
  public final hydra.langs.kusto.kql.DurationUnit unit;
  
  public Duration (Integer value, hydra.langs.kusto.kql.DurationUnit unit) {
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
  
  public Duration withUnit(hydra.langs.kusto.kql.DurationUnit unit) {
    java.util.Objects.requireNonNull((unit));
    return new Duration(value, unit);
  }
}