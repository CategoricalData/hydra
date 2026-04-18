// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class DateFunction implements Serializable, Comparable<DateFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DateFunction");

  public static final hydra.core.Name CURRENT_DATE = new hydra.core.Name("currentDate");

  public static final hydra.core.Name DATE_WITH_PARAMS = new hydra.core.Name("dateWithParams");

  private DateFunction () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(CurrentDate instance) ;

    R visit(DateWithParams instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DateFunction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(CurrentDate instance) {
      return otherwise(instance);
    }

    default R visit(DateWithParams instance) {
      return otherwise(instance);
    }
  }

  public static final class CurrentDate extends openGql.grammar.DateFunction implements Serializable {
    public CurrentDate () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CurrentDate)) {
        return false;
      }
      CurrentDate o = (CurrentDate) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(DateFunction other) {
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

  public static final class DateWithParams extends openGql.grammar.DateFunction implements Serializable {
    public final hydra.util.Maybe<openGql.grammar.DateFunctionParameters> value;

    public DateWithParams (hydra.util.Maybe<openGql.grammar.DateFunctionParameters> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DateWithParams)) {
        return false;
      }
      DateWithParams o = (DateWithParams) other;
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
    public int compareTo(DateFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DateWithParams o = (DateWithParams) other;
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
