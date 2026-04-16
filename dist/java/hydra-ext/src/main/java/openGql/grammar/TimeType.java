// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class TimeType implements Serializable, Comparable<TimeType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TimeType");

  public static final hydra.core.Name ZONED_TIME = new hydra.core.Name("zonedTime");

  public static final hydra.core.Name TIME_WITH_TIME_ZONE = new hydra.core.Name("timeWithTimeZone");

  private TimeType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ZonedTime instance) ;

    R visit(TimeWithTimeZone instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TimeType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ZonedTime instance) {
      return otherwise(instance);
    }

    default R visit(TimeWithTimeZone instance) {
      return otherwise(instance);
    }
  }

  public static final class ZonedTime extends openGql.grammar.TimeType implements Serializable {
    public final openGql.grammar.ZonedTimeType value;

    public ZonedTime (openGql.grammar.ZonedTimeType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ZonedTime)) {
        return false;
      }
      ZonedTime o = (ZonedTime) other;
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
    public int compareTo(TimeType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ZonedTime o = (ZonedTime) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class TimeWithTimeZone extends openGql.grammar.TimeType implements Serializable {
    public final openGql.grammar.TimeWithTimeZoneType value;

    public TimeWithTimeZone (openGql.grammar.TimeWithTimeZoneType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TimeWithTimeZone)) {
        return false;
      }
      TimeWithTimeZone o = (TimeWithTimeZone) other;
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
    public int compareTo(TimeType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TimeWithTimeZone o = (TimeWithTimeZone) other;
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
