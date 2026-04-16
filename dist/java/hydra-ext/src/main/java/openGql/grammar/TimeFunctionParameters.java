// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class TimeFunctionParameters implements Serializable, Comparable<TimeFunctionParameters> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TimeFunctionParameters");

  public static final hydra.core.Name TIME_STRING = new hydra.core.Name("timeString");

  public static final hydra.core.Name RECORD_CONSTRUCTOR = new hydra.core.Name("recordConstructor");

  private TimeFunctionParameters () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(TimeString instance) ;

    R visit(RecordConstructor instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TimeFunctionParameters instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(TimeString instance) {
      return otherwise(instance);
    }

    default R visit(RecordConstructor instance) {
      return otherwise(instance);
    }
  }

  public static final class TimeString extends openGql.grammar.TimeFunctionParameters implements Serializable {
    public final String value;

    public TimeString (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TimeString)) {
        return false;
      }
      TimeString o = (TimeString) other;
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
    public int compareTo(TimeFunctionParameters other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TimeString o = (TimeString) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class RecordConstructor extends openGql.grammar.TimeFunctionParameters implements Serializable {
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
    public int compareTo(TimeFunctionParameters other) {
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
