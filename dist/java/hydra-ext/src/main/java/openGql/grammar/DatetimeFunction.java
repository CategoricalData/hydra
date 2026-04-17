// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class DatetimeFunction implements Serializable, Comparable<DatetimeFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DatetimeFunction");

  public static final hydra.core.Name CURRENT_TIMESTAMP = new hydra.core.Name("currentTimestamp");

  public static final hydra.core.Name ZONED_DATETIME_WITH_PARAMS = new hydra.core.Name("zonedDatetimeWithParams");

  private DatetimeFunction () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(CurrentTimestamp instance) ;

    R visit(ZonedDatetimeWithParams instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DatetimeFunction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(CurrentTimestamp instance) {
      return otherwise(instance);
    }

    default R visit(ZonedDatetimeWithParams instance) {
      return otherwise(instance);
    }
  }

  public static final class CurrentTimestamp extends openGql.grammar.DatetimeFunction implements Serializable {
    public CurrentTimestamp () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CurrentTimestamp)) {
        return false;
      }
      CurrentTimestamp o = (CurrentTimestamp) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(DatetimeFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ZonedDatetimeWithParams extends openGql.grammar.DatetimeFunction implements Serializable {
    public final hydra.util.Maybe<openGql.grammar.DatetimeFunctionParameters> value;

    public ZonedDatetimeWithParams (hydra.util.Maybe<openGql.grammar.DatetimeFunctionParameters> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ZonedDatetimeWithParams)) {
        return false;
      }
      ZonedDatetimeWithParams o = (ZonedDatetimeWithParams) other;
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
    public int compareTo(DatetimeFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ZonedDatetimeWithParams o = (ZonedDatetimeWithParams) other;
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
