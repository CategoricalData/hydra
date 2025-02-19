// Note: this is an automatically generated file. Do not edit.

package hydra.query;

import java.io.Serializable;

/**
 * A range from min to max, inclusive
 */
public class Range implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.query.Range");
  
  public static final hydra.core.Name FIELD_NAME_MIN = new hydra.core.Name("min");
  
  public static final hydra.core.Name FIELD_NAME_MAX = new hydra.core.Name("max");
  
  public final Integer min;
  
  public final Integer max;
  
  public Range (Integer min, Integer max) {
    java.util.Objects.requireNonNull((min));
    java.util.Objects.requireNonNull((max));
    this.min = min;
    this.max = max;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Range)) {
      return false;
    }
    Range o = (Range) (other);
    return min.equals(o.min) && max.equals(o.max);
  }
  
  @Override
  public int hashCode() {
    return 2 * min.hashCode() + 3 * max.hashCode();
  }
  
  public Range withMin(Integer min) {
    java.util.Objects.requireNonNull((min));
    return new Range(min, max);
  }
  
  public Range withMax(Integer max) {
    java.util.Objects.requireNonNull((max));
    return new Range(min, max);
  }
}