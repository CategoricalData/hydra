// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class UnsignedNumericLiteral implements Serializable, Comparable<UnsignedNumericLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.UnsignedNumericLiteral");

  public static final hydra.core.Name EXACT = new hydra.core.Name("exact");

  public static final hydra.core.Name APPROXIMATE = new hydra.core.Name("approximate");

  private UnsignedNumericLiteral () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Exact instance) ;

    R visit(Approximate instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UnsignedNumericLiteral instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Exact instance) {
      return otherwise(instance);
    }

    default R visit(Approximate instance) {
      return otherwise(instance);
    }
  }

  public static final class Exact extends openGql.grammar.UnsignedNumericLiteral implements Serializable {
    public final openGql.grammar.ExactNumericLiteral value;

    public Exact (openGql.grammar.ExactNumericLiteral value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Exact)) {
        return false;
      }
      Exact o = (Exact) other;
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
    public int compareTo(UnsignedNumericLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Exact o = (Exact) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Approximate extends openGql.grammar.UnsignedNumericLiteral implements Serializable {
    public final openGql.grammar.ApproximateNumericLiteral value;

    public Approximate (openGql.grammar.ApproximateNumericLiteral value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Approximate)) {
        return false;
      }
      Approximate o = (Approximate) other;
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
    public int compareTo(UnsignedNumericLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Approximate o = (Approximate) other;
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
