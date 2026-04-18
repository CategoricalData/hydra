// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ExactNumericType implements Serializable, Comparable<ExactNumericType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ExactNumericType");

  public static final hydra.core.Name BINARY = new hydra.core.Name("binary");

  public static final hydra.core.Name DECIMAL = new hydra.core.Name("decimal");

  private ExactNumericType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Binary instance) ;

    R visit(Decimal instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ExactNumericType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Binary instance) {
      return otherwise(instance);
    }

    default R visit(Decimal instance) {
      return otherwise(instance);
    }
  }

  public static final class Binary extends openGql.grammar.ExactNumericType implements Serializable {
    public final openGql.grammar.BinaryExactNumericType value;

    public Binary (openGql.grammar.BinaryExactNumericType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Binary)) {
        return false;
      }
      Binary o = (Binary) other;
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
    public int compareTo(ExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Binary o = (Binary) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Decimal extends openGql.grammar.ExactNumericType implements Serializable {
    public final hydra.util.Maybe<openGql.grammar.PrecisionAndScale> value;

    public Decimal (hydra.util.Maybe<openGql.grammar.PrecisionAndScale> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Decimal)) {
        return false;
      }
      Decimal o = (Decimal) other;
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
    public int compareTo(ExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Decimal o = (Decimal) other;
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
