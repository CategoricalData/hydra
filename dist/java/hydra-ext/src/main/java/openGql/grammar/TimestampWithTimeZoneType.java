// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class TimestampWithTimeZoneType implements Serializable, Comparable<TimestampWithTimeZoneType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TimestampWithTimeZoneType");

  public static final hydra.core.Name NOT_NULL = new hydra.core.Name("notNull");

  public final Boolean notNull;

  public TimestampWithTimeZoneType (Boolean notNull) {
    this.notNull = notNull;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TimestampWithTimeZoneType)) {
      return false;
    }
    TimestampWithTimeZoneType o = (TimestampWithTimeZoneType) other;
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
  public int compareTo(TimestampWithTimeZoneType other) {
    return hydra.util.Comparing.compare(
      notNull,
      other.notNull);
  }
}
