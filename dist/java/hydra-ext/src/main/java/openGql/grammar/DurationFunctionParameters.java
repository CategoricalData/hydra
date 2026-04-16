// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class DurationFunctionParameters implements Serializable, Comparable<DurationFunctionParameters> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DurationFunctionParameters");

  public static final hydra.core.Name DURATION_STRING = new hydra.core.Name("durationString");

  public static final hydra.core.Name RECORD_CONSTRUCTOR = new hydra.core.Name("recordConstructor");

  private DurationFunctionParameters () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(DurationString instance) ;

    R visit(RecordConstructor instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DurationFunctionParameters instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(DurationString instance) {
      return otherwise(instance);
    }

    default R visit(RecordConstructor instance) {
      return otherwise(instance);
    }
  }

  public static final class DurationString extends openGql.grammar.DurationFunctionParameters implements Serializable {
    public final String value;

    public DurationString (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DurationString)) {
        return false;
      }
      DurationString o = (DurationString) other;
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
    public int compareTo(DurationFunctionParameters other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DurationString o = (DurationString) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class RecordConstructor extends openGql.grammar.DurationFunctionParameters implements Serializable {
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
    public int compareTo(DurationFunctionParameters other) {
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
