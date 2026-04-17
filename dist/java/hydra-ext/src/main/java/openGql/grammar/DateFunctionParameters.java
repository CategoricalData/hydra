// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class DateFunctionParameters implements Serializable, Comparable<DateFunctionParameters> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DateFunctionParameters");

  public static final hydra.core.Name DATE_STRING = new hydra.core.Name("dateString");

  public static final hydra.core.Name RECORD_CONSTRUCTOR = new hydra.core.Name("recordConstructor");

  private DateFunctionParameters () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(DateString instance) ;

    R visit(RecordConstructor instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DateFunctionParameters instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(DateString instance) {
      return otherwise(instance);
    }

    default R visit(RecordConstructor instance) {
      return otherwise(instance);
    }
  }

  public static final class DateString extends openGql.grammar.DateFunctionParameters implements Serializable {
    public final String value;

    public DateString (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DateString)) {
        return false;
      }
      DateString o = (DateString) other;
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
    public int compareTo(DateFunctionParameters other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DateString o = (DateString) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class RecordConstructor extends openGql.grammar.DateFunctionParameters implements Serializable {
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
    public int compareTo(DateFunctionParameters other) {
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
