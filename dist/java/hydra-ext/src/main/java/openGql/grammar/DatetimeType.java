// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class DatetimeType implements Serializable, Comparable<DatetimeType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DatetimeType");

  public static final hydra.core.Name ZONED_DATETIME = new hydra.core.Name("zonedDatetime");

  public static final hydra.core.Name TIMESTAMP_WITH_TIME_ZONE = new hydra.core.Name("timestampWithTimeZone");

  private DatetimeType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ZonedDatetime instance) ;

    R visit(TimestampWithTimeZone instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DatetimeType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ZonedDatetime instance) {
      return otherwise(instance);
    }

    default R visit(TimestampWithTimeZone instance) {
      return otherwise(instance);
    }
  }

  public static final class ZonedDatetime extends openGql.grammar.DatetimeType implements Serializable {
    public final openGql.grammar.ZonedDatetimeType value;

    public ZonedDatetime (openGql.grammar.ZonedDatetimeType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ZonedDatetime)) {
        return false;
      }
      ZonedDatetime o = (ZonedDatetime) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(DatetimeType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ZonedDatetime o = (ZonedDatetime) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class TimestampWithTimeZone extends openGql.grammar.DatetimeType implements Serializable {
    public final openGql.grammar.TimestampWithTimeZoneType value;

    public TimestampWithTimeZone (openGql.grammar.TimestampWithTimeZoneType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TimestampWithTimeZone)) {
        return false;
      }
      TimestampWithTimeZone o = (TimestampWithTimeZone) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(DatetimeType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TimestampWithTimeZone o = (TimestampWithTimeZone) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
