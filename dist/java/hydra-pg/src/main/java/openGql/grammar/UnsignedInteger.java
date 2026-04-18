// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class UnsignedInteger implements Serializable, Comparable<UnsignedInteger> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.UnsignedInteger");

  public static final hydra.core.Name DECIMAL = new hydra.core.Name("decimal");

  public static final hydra.core.Name HEXADECIMAL = new hydra.core.Name("hexadecimal");

  public static final hydra.core.Name OCTAL = new hydra.core.Name("octal");

  public static final hydra.core.Name BINARY = new hydra.core.Name("binary");

  private UnsignedInteger () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Decimal instance) ;

    R visit(Hexadecimal instance) ;

    R visit(Octal instance) ;

    R visit(Binary instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UnsignedInteger instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Decimal instance) {
      return otherwise(instance);
    }

    default R visit(Hexadecimal instance) {
      return otherwise(instance);
    }

    default R visit(Octal instance) {
      return otherwise(instance);
    }

    default R visit(Binary instance) {
      return otherwise(instance);
    }
  }

  public static final class Decimal extends openGql.grammar.UnsignedInteger implements Serializable {
    public final String value;

    public Decimal (String value) {
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
    public int compareTo(UnsignedInteger other) {
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

  public static final class Hexadecimal extends openGql.grammar.UnsignedInteger implements Serializable {
    public final String value;

    public Hexadecimal (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Hexadecimal)) {
        return false;
      }
      Hexadecimal o = (Hexadecimal) other;
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
    public int compareTo(UnsignedInteger other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Hexadecimal o = (Hexadecimal) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Octal extends openGql.grammar.UnsignedInteger implements Serializable {
    public final String value;

    public Octal (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Octal)) {
        return false;
      }
      Octal o = (Octal) other;
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
    public int compareTo(UnsignedInteger other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Octal o = (Octal) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Binary extends openGql.grammar.UnsignedInteger implements Serializable {
    public final String value;

    public Binary (String value) {
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
    public int compareTo(UnsignedInteger other) {
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
}
