// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class BinaryExactNumericType implements Serializable, Comparable<BinaryExactNumericType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.BinaryExactNumericType");

  public static final hydra.core.Name SIGNED = new hydra.core.Name("signed");

  public static final hydra.core.Name UNSIGNED = new hydra.core.Name("unsigned");

  private BinaryExactNumericType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Signed instance) ;

    R visit(Unsigned instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BinaryExactNumericType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Signed instance) {
      return otherwise(instance);
    }

    default R visit(Unsigned instance) {
      return otherwise(instance);
    }
  }

  public static final class Signed extends openGql.grammar.BinaryExactNumericType implements Serializable {
    public final openGql.grammar.SignedBinaryExactNumericType value;

    public Signed (openGql.grammar.SignedBinaryExactNumericType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Signed)) {
        return false;
      }
      Signed o = (Signed) other;
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
    public int compareTo(BinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Signed o = (Signed) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Unsigned extends openGql.grammar.BinaryExactNumericType implements Serializable {
    public final openGql.grammar.UnsignedBinaryExactNumericType value;

    public Unsigned (openGql.grammar.UnsignedBinaryExactNumericType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unsigned)) {
        return false;
      }
      Unsigned o = (Unsigned) other;
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
    public int compareTo(BinaryExactNumericType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Unsigned o = (Unsigned) other;
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
