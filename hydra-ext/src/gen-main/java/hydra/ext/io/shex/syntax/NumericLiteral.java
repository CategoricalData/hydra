// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class NumericLiteral implements Serializable, Comparable<NumericLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.NumericLiteral");

  public static final hydra.core.Name INTEGER = new hydra.core.Name("Integer");

  public static final hydra.core.Name DECIMAL = new hydra.core.Name("Decimal");

  public static final hydra.core.Name DOUBLE = new hydra.core.Name("Double");

  private NumericLiteral () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Integer_ instance) ;

    R visit(Decimal instance) ;

    R visit(Double_ instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NumericLiteral instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Integer_ instance) {
      return otherwise(instance);
    }

    default R visit(Decimal instance) {
      return otherwise(instance);
    }

    default R visit(Double_ instance) {
      return otherwise(instance);
    }
  }

  public static final class Integer_ extends hydra.ext.io.shex.syntax.NumericLiteral implements Serializable {
    public final hydra.ext.io.shex.syntax.Integer_ value;

    public Integer_ (hydra.ext.io.shex.syntax.Integer_ value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Integer_)) {
        return false;
      }
      Integer_ o = (Integer_) other;
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
    public int compareTo(NumericLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Integer_ o = (Integer_) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Decimal extends hydra.ext.io.shex.syntax.NumericLiteral implements Serializable {
    public final hydra.ext.io.shex.syntax.Decimal value;

    public Decimal (hydra.ext.io.shex.syntax.Decimal value) {
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
    public int compareTo(NumericLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Decimal o = (Decimal) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Double_ extends hydra.ext.io.shex.syntax.NumericLiteral implements Serializable {
    public final hydra.ext.io.shex.syntax.Double_ value;

    public Double_ (hydra.ext.io.shex.syntax.Double_ value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Double_)) {
        return false;
      }
      Double_ o = (Double_) other;
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
    public int compareTo(NumericLiteral other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Double_ o = (Double_) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
