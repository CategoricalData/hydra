// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class DurationValueFunction implements Serializable, Comparable<DurationValueFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DurationValueFunction");

  public static final hydra.core.Name DURATION_FUNCTION = new hydra.core.Name("durationFunction");

  public static final hydra.core.Name ABSOLUTE_VALUE = new hydra.core.Name("absoluteValue");

  private DurationValueFunction () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(DurationFunction instance) ;

    R visit(AbsoluteValue instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DurationValueFunction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(DurationFunction instance) {
      return otherwise(instance);
    }

    default R visit(AbsoluteValue instance) {
      return otherwise(instance);
    }
  }

  public static final class DurationFunction extends openGql.grammar.DurationValueFunction implements Serializable {
    public final openGql.grammar.DurationFunctionParameters value;

    public DurationFunction (openGql.grammar.DurationFunctionParameters value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DurationFunction)) {
        return false;
      }
      DurationFunction o = (DurationFunction) other;
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
    public int compareTo(DurationValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DurationFunction o = (DurationFunction) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class AbsoluteValue extends openGql.grammar.DurationValueFunction implements Serializable {
    public final openGql.grammar.ValueExpression value;

    public AbsoluteValue (openGql.grammar.ValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AbsoluteValue)) {
        return false;
      }
      AbsoluteValue o = (AbsoluteValue) other;
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
    public int compareTo(DurationValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AbsoluteValue o = (AbsoluteValue) other;
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
