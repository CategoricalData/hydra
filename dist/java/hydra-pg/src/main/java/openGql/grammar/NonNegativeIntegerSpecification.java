// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class NonNegativeIntegerSpecification implements Serializable, Comparable<NonNegativeIntegerSpecification> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NonNegativeIntegerSpecification");

  public static final hydra.core.Name UNSIGNED_INTEGER = new hydra.core.Name("unsignedInteger");

  public static final hydra.core.Name DYNAMIC_PARAMETER_SPECIFICATION = new hydra.core.Name("dynamicParameterSpecification");

  private NonNegativeIntegerSpecification () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(UnsignedInteger instance) ;

    R visit(DynamicParameterSpecification instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NonNegativeIntegerSpecification instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(UnsignedInteger instance) {
      return otherwise(instance);
    }

    default R visit(DynamicParameterSpecification instance) {
      return otherwise(instance);
    }
  }

  public static final class UnsignedInteger extends openGql.grammar.NonNegativeIntegerSpecification implements Serializable {
    public final openGql.grammar.UnsignedInteger value;

    public UnsignedInteger (openGql.grammar.UnsignedInteger value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnsignedInteger)) {
        return false;
      }
      UnsignedInteger o = (UnsignedInteger) other;
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
    public int compareTo(NonNegativeIntegerSpecification other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UnsignedInteger o = (UnsignedInteger) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DynamicParameterSpecification extends openGql.grammar.NonNegativeIntegerSpecification implements Serializable {
    public final String value;

    public DynamicParameterSpecification (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DynamicParameterSpecification)) {
        return false;
      }
      DynamicParameterSpecification o = (DynamicParameterSpecification) other;
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
    public int compareTo(NonNegativeIntegerSpecification other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DynamicParameterSpecification o = (DynamicParameterSpecification) other;
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
