// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class TimeWithoutTimeZoneType implements Serializable, Comparable<TimeWithoutTimeZoneType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TimeWithoutTimeZoneType");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean notNull;

  public TimeWithoutTimeZoneType (Boolean notNull) {
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TimeWithoutTimeZoneType)) {
      return false;
    }
    TimeWithoutTimeZoneType o = (TimeWithoutTimeZoneType) other;
    return java.util.Objects.equals(
      this.notNull,
      o.notNull);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(notNull);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TimeWithoutTimeZoneType other) {
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }
}
