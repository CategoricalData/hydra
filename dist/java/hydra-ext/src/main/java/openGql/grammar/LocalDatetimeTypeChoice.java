// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class LocalDatetimeTypeChoice implements Serializable, Comparable<LocalDatetimeTypeChoice> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.LocalDatetimeTypeChoice");

  public static final hydra.core.Name LOCAL_DATETIME = new hydra.core.Name("localDatetime");

  public static final hydra.core.Name TIMESTAMP_WITHOUT_TIME_ZONE = new hydra.core.Name("timestampWithoutTimeZone");

  private LocalDatetimeTypeChoice () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(LocalDatetime instance) ;

    R visit(TimestampWithoutTimeZone instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LocalDatetimeTypeChoice instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(LocalDatetime instance) {
      return otherwise(instance);
    }

    default R visit(TimestampWithoutTimeZone instance) {
      return otherwise(instance);
    }
  }

  public static final class LocalDatetime extends openGql.grammar.LocalDatetimeTypeChoice implements Serializable {
    public final openGql.grammar.LocalDatetimeType value;

    public LocalDatetime (openGql.grammar.LocalDatetimeType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LocalDatetime)) {
        return false;
      }
      LocalDatetime o = (LocalDatetime) other;
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
    public int compareTo(LocalDatetimeTypeChoice other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LocalDatetime o = (LocalDatetime) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class TimestampWithoutTimeZone extends openGql.grammar.LocalDatetimeTypeChoice implements Serializable {
    public final openGql.grammar.TimestampWithoutTimeZoneType value;

    public TimestampWithoutTimeZone (openGql.grammar.TimestampWithoutTimeZoneType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TimestampWithoutTimeZone)) {
        return false;
      }
      TimestampWithoutTimeZone o = (TimestampWithoutTimeZone) other;
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
    public int compareTo(LocalDatetimeTypeChoice other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TimestampWithoutTimeZone o = (TimestampWithoutTimeZone) other;
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
