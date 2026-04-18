// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public abstract class Parameter implements Serializable, Comparable<Parameter> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.Parameter");

  public static final hydra.core.Name SYMBOLIC = new hydra.core.Name("symbolic");

  public static final hydra.core.Name INTEGER = new hydra.core.Name("integer");

  private Parameter () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Symbolic instance) ;

    R visit(Integer_ instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Parameter instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Symbolic instance) {
      return otherwise(instance);
    }

    default R visit(Integer_ instance) {
      return otherwise(instance);
    }
  }

  public static final class Symbolic extends hydra.cypher.openCypher.Parameter implements Serializable {
    public final String value;

    public Symbolic (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Symbolic)) {
        return false;
      }
      Symbolic o = (Symbolic) other;
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
    public int compareTo(Parameter other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Symbolic o = (Symbolic) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Integer_ extends hydra.cypher.openCypher.Parameter implements Serializable {
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
    public int compareTo(Parameter other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Integer_ o = (Integer_) other;
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
