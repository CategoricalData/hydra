// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class TimestampWithoutTimeZoneType implements Serializable, Comparable<TimestampWithoutTimeZoneType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TimestampWithoutTimeZoneType");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean notNull;

  public TimestampWithoutTimeZoneType (Boolean notNull) {
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TimestampWithoutTimeZoneType)) {
      return false;
    }
    TimestampWithoutTimeZoneType o = (TimestampWithoutTimeZoneType) other;
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
  public int compareTo(TimestampWithoutTimeZoneType other) {
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }
}
