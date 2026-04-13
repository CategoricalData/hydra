// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class DateAddArgs implements Serializable, Comparable<DateAddArgs> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.DateAddArgs");

  public static final hydra.core.Name UNIT = new hydra.core.Name("unit");

  public static final hydra.core.Name DURATION = new hydra.core.Name("duration");

  public final hydra.tinkerpop.gremlin.TraversalDTArgument unit;

  public final hydra.tinkerpop.gremlin.IntegerArgument duration;

  public DateAddArgs (hydra.tinkerpop.gremlin.TraversalDTArgument unit, hydra.tinkerpop.gremlin.IntegerArgument duration) {
    this.unit = unit;
    this.duration = duration;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DateAddArgs)) {
      return false;
    }
    DateAddArgs o = (DateAddArgs) other;
    return java.util.Objects.equals(
      this.unit,
      o.unit) && java.util.Objects.equals(
      this.duration,
      o.duration);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(unit) + 3 * java.util.Objects.hashCode(duration);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DateAddArgs other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      unit,
      other.unit);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      duration,
      other.duration);
  }

  public DateAddArgs withUnit(hydra.tinkerpop.gremlin.TraversalDTArgument unit) {
    return new DateAddArgs(unit, duration);
  }

  public DateAddArgs withDuration(hydra.tinkerpop.gremlin.IntegerArgument duration) {
    return new DateAddArgs(unit, duration);
  }
}
