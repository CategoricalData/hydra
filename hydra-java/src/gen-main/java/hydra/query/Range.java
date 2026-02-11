// Note: this is an automatically generated file. Do not edit.

package hydra.query;

import java.io.Serializable;

/**
 * A range from min to max, inclusive
 */
public class Range implements Serializable, Comparable<Range> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.query.Range");
  
  public static final hydra.core.Name FIELD_NAME_MIN = new hydra.core.Name("min");
  
  public static final hydra.core.Name FIELD_NAME_MAX = new hydra.core.Name("max");
  
  /**
   * The minimum value (inclusive)
   */
  public final Integer min;
  
  /**
   * The maximum value (inclusive)
   */
  public final Integer max;
  
  public Range (Integer min, Integer max) {
    this.min = min;
    this.max = max;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Range)) {
      return false;
    }
    Range o = (Range) other;
    return java.util.Objects.equals(
      this.min,
      o.min) && java.util.Objects.equals(
      this.max,
      o.max);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(min) + 3 * java.util.Objects.hashCode(max);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Range other) {
    int cmp = 0;
    cmp = ((Comparable) min).compareTo(other.min);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) max).compareTo(other.max);
  }
  
  public Range withMin(Integer min) {
    return new Range(min, max);
  }
  
  public Range withMax(Integer max) {
    return new Range(min, max);
  }
}
