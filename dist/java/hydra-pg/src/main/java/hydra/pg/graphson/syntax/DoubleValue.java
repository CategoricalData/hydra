// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public abstract class DoubleValue implements Serializable, Comparable<DoubleValue> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.graphson.syntax.DoubleValue");

  public static final hydra.core.Name FINITE = new hydra.core.Name("finite");

  public static final hydra.core.Name INFINITY = new hydra.core.Name("infinity");

  public static final hydra.core.Name NEGATIVE_INFINITY = new hydra.core.Name("negativeInfinity");

  public static final hydra.core.Name NOT_A_NUMBER = new hydra.core.Name("notANumber");

  private DoubleValue () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Finite instance) ;

    R visit(Infinity instance) ;

    R visit(NegativeInfinity instance) ;

    R visit(NotANumber instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DoubleValue instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Finite instance) {
      return otherwise(instance);
    }

    default R visit(Infinity instance) {
      return otherwise(instance);
    }

    default R visit(NegativeInfinity instance) {
      return otherwise(instance);
    }

    default R visit(NotANumber instance) {
      return otherwise(instance);
    }
  }

  public static final class Finite extends hydra.pg.graphson.syntax.DoubleValue implements Serializable {
    public final Double value;

    public Finite (Double value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Finite)) {
        return false;
      }
      Finite o = (Finite) other;
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
    public int compareTo(DoubleValue other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Finite o = (Finite) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Infinity extends hydra.pg.graphson.syntax.DoubleValue implements Serializable {
    public Infinity () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Infinity)) {
        return false;
      }
      Infinity o = (Infinity) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(DoubleValue other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class NegativeInfinity extends hydra.pg.graphson.syntax.DoubleValue implements Serializable {
    public NegativeInfinity () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NegativeInfinity)) {
        return false;
      }
      NegativeInfinity o = (NegativeInfinity) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(DoubleValue other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class NotANumber extends hydra.pg.graphson.syntax.DoubleValue implements Serializable {
    public NotANumber () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotANumber)) {
        return false;
      }
      NotANumber o = (NotANumber) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(DoubleValue other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
