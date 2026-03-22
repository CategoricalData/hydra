// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class NumberLiteral implements Serializable, Comparable<NumberLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.NumberLiteral");

  public static final hydra.core.Name DOUBLE = new hydra.core.Name("double");

  public static final hydra.core.Name INTEGER = new hydra.core.Name("integer");

  private NumberLiteral () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Double_ instance) ;

    R visit(Integer_ instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NumberLiteral instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Double_ instance) {
      return otherwise(instance);
    }

    default R visit(Integer_ instance) {
      return otherwise(instance);
    }
  }

  public static final class Double_ extends hydra.ext.cypher.openCypher.NumberLiteral implements Serializable {
    public final Double value;

    public Double_ (Double value) {
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
    public int compareTo(NumberLiteral other) {
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

  public static final class Integer_ extends hydra.ext.cypher.openCypher.NumberLiteral implements Serializable {
    public final java.math.BigInteger value;

    public Integer_ (java.math.BigInteger value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Integer_)) {
        return false;
      }
      Integer_ o = (Integer_) other;
      return this.value.compareTo(o.value) == 0;
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(NumberLiteral other) {
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
}
