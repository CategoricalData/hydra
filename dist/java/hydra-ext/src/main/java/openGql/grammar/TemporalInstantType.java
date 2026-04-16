// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class TemporalInstantType implements Serializable, Comparable<TemporalInstantType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TemporalInstantType");

  public static final hydra.core.Name DATETIME_TYPE = new hydra.core.Name("datetimeType");

  public static final hydra.core.Name LOCALDATETIME_TYPE = new hydra.core.Name("localdatetimeType");

  public static final hydra.core.Name DATE_TYPE = new hydra.core.Name("dateType");

  public static final hydra.core.Name TIME_TYPE = new hydra.core.Name("timeType");

  public static final hydra.core.Name LOCALTIME_TYPE = new hydra.core.Name("localtimeType");

  private TemporalInstantType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(DatetimeType instance) ;

    R visit(LocaldatetimeType instance) ;

    R visit(DateType instance) ;

    R visit(TimeType instance) ;

    R visit(LocaltimeType instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TemporalInstantType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(DatetimeType instance) {
      return otherwise(instance);
    }

    default R visit(LocaldatetimeType instance) {
      return otherwise(instance);
    }

    default R visit(DateType instance) {
      return otherwise(instance);
    }

    default R visit(TimeType instance) {
      return otherwise(instance);
    }

    default R visit(LocaltimeType instance) {
      return otherwise(instance);
    }
  }

  public static final class DatetimeType extends openGql.grammar.TemporalInstantType implements Serializable {
    public final openGql.grammar.DatetimeType value;

    public DatetimeType (openGql.grammar.DatetimeType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DatetimeType)) {
        return false;
      }
      DatetimeType o = (DatetimeType) other;
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
    public int compareTo(TemporalInstantType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DatetimeType o = (DatetimeType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class LocaldatetimeType extends openGql.grammar.TemporalInstantType implements Serializable {
    public final openGql.grammar.LocaldatetimeType value;

    public LocaldatetimeType (openGql.grammar.LocaldatetimeType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LocaldatetimeType)) {
        return false;
      }
      LocaldatetimeType o = (LocaldatetimeType) other;
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
    public int compareTo(TemporalInstantType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LocaldatetimeType o = (LocaldatetimeType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DateType extends openGql.grammar.TemporalInstantType implements Serializable {
    public final openGql.grammar.DateType value;

    public DateType (openGql.grammar.DateType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DateType)) {
        return false;
      }
      DateType o = (DateType) other;
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
    public int compareTo(TemporalInstantType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DateType o = (DateType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class TimeType extends openGql.grammar.TemporalInstantType implements Serializable {
    public final openGql.grammar.TimeType value;

    public TimeType (openGql.grammar.TimeType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TimeType)) {
        return false;
      }
      TimeType o = (TimeType) other;
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
    public int compareTo(TemporalInstantType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TimeType o = (TimeType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class LocaltimeType extends openGql.grammar.TemporalInstantType implements Serializable {
    public final openGql.grammar.LocaltimeType value;

    public LocaltimeType (openGql.grammar.LocaltimeType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LocaltimeType)) {
        return false;
      }
      LocaltimeType o = (LocaltimeType) other;
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
    public int compareTo(TemporalInstantType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LocaltimeType o = (LocaltimeType) other;
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
