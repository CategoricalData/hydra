// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class NumericLiteral implements Serializable, Comparable<NumericLiteral> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.NumericLiteral");

  public static final hydra.core.Name INTEGER = new hydra.core.Name("integer");

  public static final hydra.core.Name FLOAT = new hydra.core.Name("float");

  private NumericLiteral () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Integer_ instance) ;

    R visit(Float_ instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NumericLiteral instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Integer_ instance) {
      return otherwise(instance);
    }

    default R visit(Float_ instance) {
      return otherwise(instance);
    }
  }

  public static final class Integer_ extends hydra.ext.org.apache.tinkerpop.gremlin.NumericLiteral implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.IntegerLiteral value;

    public Integer_ (hydra.ext.org.apache.tinkerpop.gremlin.IntegerLiteral value) {
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

  public static final class Float_ extends hydra.ext.org.apache.tinkerpop.gremlin.NumericLiteral implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.FloatLiteral value;

    public Float_ (hydra.ext.org.apache.tinkerpop.gremlin.FloatLiteral value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Float_)) {
        return false;
      }
      Float_ o = (Float_) other;
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
      Float_ o = (Float_) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
