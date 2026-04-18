// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class LocaldatetimeFunction implements Serializable, Comparable<LocaldatetimeFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.LocaldatetimeFunction");

  public static final hydra.core.Name LOCAL_TIMESTAMP = new hydra.core.Name("localTimestamp");

  public static final hydra.core.Name LOCAL_DATETIME_WITH_PARAMS = new hydra.core.Name("localDatetimeWithParams");

  private LocaldatetimeFunction () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(LocalTimestamp instance) ;

    R visit(LocalDatetimeWithParams instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LocaldatetimeFunction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(LocalTimestamp instance) {
      return otherwise(instance);
    }

    default R visit(LocalDatetimeWithParams instance) {
      return otherwise(instance);
    }
  }

  public static final class LocalTimestamp extends openGql.grammar.LocaldatetimeFunction implements Serializable {
    public LocalTimestamp () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LocalTimestamp)) {
        return false;
      }
      LocalTimestamp o = (LocalTimestamp) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(LocaldatetimeFunction other) {
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

  public static final class LocalDatetimeWithParams extends openGql.grammar.LocaldatetimeFunction implements Serializable {
    public final hydra.util.Maybe<openGql.grammar.DatetimeFunctionParameters> value;

    public LocalDatetimeWithParams (hydra.util.Maybe<openGql.grammar.DatetimeFunctionParameters> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LocalDatetimeWithParams)) {
        return false;
      }
      LocalDatetimeWithParams o = (LocalDatetimeWithParams) other;
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
    public int compareTo(LocaldatetimeFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LocalDatetimeWithParams o = (LocalDatetimeWithParams) other;
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
