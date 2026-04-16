// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class UnsignedLiteral implements Serializable, Comparable<UnsignedLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.UnsignedLiteral");

  public static final hydra.core.Name NUMERIC = new hydra.core.Name("numeric");

  public static final hydra.core.Name GENERAL = new hydra.core.Name("general");

  private UnsignedLiteral () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Numeric instance) ;

    R visit(General instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UnsignedLiteral instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Numeric instance) {
      return otherwise(instance);
    }

    default R visit(General instance) {
      return otherwise(instance);
    }
  }

  public static final class Numeric extends openGql.grammar.UnsignedLiteral implements Serializable {
    public final openGql.grammar.UnsignedNumericLiteral value;

    public Numeric (openGql.grammar.UnsignedNumericLiteral value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Numeric)) {
        return false;
      }
      Numeric o = (Numeric) other;
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
    public int compareTo(UnsignedLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Numeric o = (Numeric) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class General extends openGql.grammar.UnsignedLiteral implements Serializable {
    public final openGql.grammar.GeneralLiteral value;

    public General (openGql.grammar.GeneralLiteral value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof General)) {
        return false;
      }
      General o = (General) other;
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
    public int compareTo(UnsignedLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      General o = (General) other;
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
