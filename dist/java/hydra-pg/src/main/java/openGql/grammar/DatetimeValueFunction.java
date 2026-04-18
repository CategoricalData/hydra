// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class DatetimeValueFunction implements Serializable, Comparable<DatetimeValueFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DatetimeValueFunction");

  public static final hydra.core.Name DATE_FUNCTION = new hydra.core.Name("dateFunction");

  public static final hydra.core.Name TIME_FUNCTION = new hydra.core.Name("timeFunction");

  public static final hydra.core.Name DATETIME_FUNCTION = new hydra.core.Name("datetimeFunction");

  public static final hydra.core.Name LOCALTIME_FUNCTION = new hydra.core.Name("localtimeFunction");

  public static final hydra.core.Name LOCALDATETIME_FUNCTION = new hydra.core.Name("localdatetimeFunction");

  private DatetimeValueFunction () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(DateFunction instance) ;

    R visit(TimeFunction instance) ;

    R visit(DatetimeFunction instance) ;

    R visit(LocaltimeFunction instance) ;

    R visit(LocaldatetimeFunction instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DatetimeValueFunction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(DateFunction instance) {
      return otherwise(instance);
    }

    default R visit(TimeFunction instance) {
      return otherwise(instance);
    }

    default R visit(DatetimeFunction instance) {
      return otherwise(instance);
    }

    default R visit(LocaltimeFunction instance) {
      return otherwise(instance);
    }

    default R visit(LocaldatetimeFunction instance) {
      return otherwise(instance);
    }
  }

  public static final class DateFunction extends openGql.grammar.DatetimeValueFunction implements Serializable {
    public final openGql.grammar.DateFunction value;

    public DateFunction (openGql.grammar.DateFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DateFunction)) {
        return false;
      }
      DateFunction o = (DateFunction) other;
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
    public int compareTo(DatetimeValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DateFunction o = (DateFunction) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class TimeFunction extends openGql.grammar.DatetimeValueFunction implements Serializable {
    public final openGql.grammar.TimeFunction value;

    public TimeFunction (openGql.grammar.TimeFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TimeFunction)) {
        return false;
      }
      TimeFunction o = (TimeFunction) other;
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
    public int compareTo(DatetimeValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TimeFunction o = (TimeFunction) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DatetimeFunction extends openGql.grammar.DatetimeValueFunction implements Serializable {
    public final openGql.grammar.DatetimeFunction value;

    public DatetimeFunction (openGql.grammar.DatetimeFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DatetimeFunction)) {
        return false;
      }
      DatetimeFunction o = (DatetimeFunction) other;
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
    public int compareTo(DatetimeValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DatetimeFunction o = (DatetimeFunction) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class LocaltimeFunction extends openGql.grammar.DatetimeValueFunction implements Serializable {
    public final hydra.util.Maybe<openGql.grammar.TimeFunctionParameters> value;

    public LocaltimeFunction (hydra.util.Maybe<openGql.grammar.TimeFunctionParameters> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LocaltimeFunction)) {
        return false;
      }
      LocaltimeFunction o = (LocaltimeFunction) other;
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
    public int compareTo(DatetimeValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LocaltimeFunction o = (LocaltimeFunction) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class LocaldatetimeFunction extends openGql.grammar.DatetimeValueFunction implements Serializable {
    public final openGql.grammar.LocaldatetimeFunction value;

    public LocaldatetimeFunction (openGql.grammar.LocaldatetimeFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LocaldatetimeFunction)) {
        return false;
      }
      LocaldatetimeFunction o = (LocaldatetimeFunction) other;
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
    public int compareTo(DatetimeValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LocaldatetimeFunction o = (LocaldatetimeFunction) other;
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
