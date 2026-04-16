// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class LocalTimeTypeChoice implements Serializable, Comparable<LocalTimeTypeChoice> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.LocalTimeTypeChoice");

  public static final hydra.core.Name LOCAL_TIME = new hydra.core.Name("localTime");

  public static final hydra.core.Name TIME_WITHOUT_TIME_ZONE = new hydra.core.Name("timeWithoutTimeZone");

  private LocalTimeTypeChoice () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(LocalTime instance) ;

    R visit(TimeWithoutTimeZone instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LocalTimeTypeChoice instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(LocalTime instance) {
      return otherwise(instance);
    }

    default R visit(TimeWithoutTimeZone instance) {
      return otherwise(instance);
    }
  }

  public static final class LocalTime extends openGql.grammar.LocalTimeTypeChoice implements Serializable {
    public final openGql.grammar.LocalTimeType value;

    public LocalTime (openGql.grammar.LocalTimeType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LocalTime)) {
        return false;
      }
      LocalTime o = (LocalTime) other;
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
    public int compareTo(LocalTimeTypeChoice other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LocalTime o = (LocalTime) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class TimeWithoutTimeZone extends openGql.grammar.LocalTimeTypeChoice implements Serializable {
    public final openGql.grammar.TimeWithoutTimeZoneType value;

    public TimeWithoutTimeZone (openGql.grammar.TimeWithoutTimeZoneType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TimeWithoutTimeZone)) {
        return false;
      }
      TimeWithoutTimeZone o = (TimeWithoutTimeZone) other;
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
    public int compareTo(LocalTimeTypeChoice other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TimeWithoutTimeZone o = (TimeWithoutTimeZone) other;
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
