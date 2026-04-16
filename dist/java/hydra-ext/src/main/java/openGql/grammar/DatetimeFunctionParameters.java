// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class DatetimeFunctionParameters implements Serializable, Comparable<DatetimeFunctionParameters> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DatetimeFunctionParameters");

  public static final hydra.core.Name DATETIME_STRING = new hydra.core.Name("datetimeString");

  public static final hydra.core.Name RECORD_CONSTRUCTOR = new hydra.core.Name("recordConstructor");

  private DatetimeFunctionParameters () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(DatetimeString instance) ;

    R visit(RecordConstructor instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DatetimeFunctionParameters instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(DatetimeString instance) {
      return otherwise(instance);
    }

    default R visit(RecordConstructor instance) {
      return otherwise(instance);
    }
  }

  public static final class DatetimeString extends openGql.grammar.DatetimeFunctionParameters implements Serializable {
    public final String value;

    public DatetimeString (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DatetimeString)) {
        return false;
      }
      DatetimeString o = (DatetimeString) other;
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
    public int compareTo(DatetimeFunctionParameters other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DatetimeString o = (DatetimeString) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class RecordConstructor extends openGql.grammar.DatetimeFunctionParameters implements Serializable {
    public final hydra.util.Maybe<java.util.List<openGql.grammar.Field>> value;

    public RecordConstructor (hydra.util.Maybe<java.util.List<openGql.grammar.Field>> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RecordConstructor)) {
        return false;
      }
      RecordConstructor o = (RecordConstructor) other;
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
    public int compareTo(DatetimeFunctionParameters other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      RecordConstructor o = (RecordConstructor) other;
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
