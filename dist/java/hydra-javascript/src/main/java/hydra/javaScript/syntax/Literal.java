// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A literal value
 */
public abstract class Literal implements Serializable, Comparable<Literal> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.Literal");

  public static final hydra.core.Name STRING = new hydra.core.Name("string");

  public static final hydra.core.Name NUMBER = new hydra.core.Name("number");

  public static final hydra.core.Name BOOLEAN = new hydra.core.Name("boolean");

  public static final hydra.core.Name NULL = new hydra.core.Name("null");

  public static final hydra.core.Name UNDEFINED = new hydra.core.Name("undefined");

  public static final hydra.core.Name BIG_INT = new hydra.core.Name("bigInt");

  public static final hydra.core.Name TEMPLATE = new hydra.core.Name("template");

  private Literal () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(String_ instance) ;

    R visit(Number_ instance) ;

    R visit(Boolean_ instance) ;

    R visit(Null instance) ;

    R visit(Undefined instance) ;

    R visit(BigInt instance) ;

    R visit(Template instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Literal instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(String_ instance) {
      return otherwise(instance);
    }

    default R visit(Number_ instance) {
      return otherwise(instance);
    }

    default R visit(Boolean_ instance) {
      return otherwise(instance);
    }

    default R visit(Null instance) {
      return otherwise(instance);
    }

    default R visit(Undefined instance) {
      return otherwise(instance);
    }

    default R visit(BigInt instance) {
      return otherwise(instance);
    }

    default R visit(Template instance) {
      return otherwise(instance);
    }
  }

  /**
   * A string literal
   */
  public static final class String_ extends hydra.javaScript.syntax.Literal implements Serializable {
    public final hydra.javaScript.syntax.StringLiteral value;

    public String_ (hydra.javaScript.syntax.StringLiteral value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) other;
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
    public int compareTo(Literal other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      String_ o = (String_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A numeric literal
   */
  public static final class Number_ extends hydra.javaScript.syntax.Literal implements Serializable {
    public final hydra.javaScript.syntax.NumericLiteral value;

    public Number_ (hydra.javaScript.syntax.NumericLiteral value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Number_)) {
        return false;
      }
      Number_ o = (Number_) other;
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
    public int compareTo(Literal other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Number_ o = (Number_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A boolean literal (true or false)
   */
  public static final class Boolean_ extends hydra.javaScript.syntax.Literal implements Serializable {
    public final Boolean value;

    public Boolean_ (Boolean value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Boolean_)) {
        return false;
      }
      Boolean_ o = (Boolean_) other;
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
    public int compareTo(Literal other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Boolean_ o = (Boolean_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * The null literal
   */
  public static final class Null extends hydra.javaScript.syntax.Literal implements Serializable {
    public Null () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Null)) {
        return false;
      }
      Null o = (Null) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Literal other) {
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

  /**
   * The undefined literal
   */
  public static final class Undefined extends hydra.javaScript.syntax.Literal implements Serializable {
    public Undefined () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Undefined)) {
        return false;
      }
      Undefined o = (Undefined) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Literal other) {
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

  /**
   * A BigInt literal (e.g., 123n)
   */
  public static final class BigInt extends hydra.javaScript.syntax.Literal implements Serializable {
    public final java.math.BigInteger value;

    public BigInt (java.math.BigInteger value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BigInt)) {
        return false;
      }
      BigInt o = (BigInt) other;
      return this.value.compareTo(o.value) == 0;
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Literal other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      BigInt o = (BigInt) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A template literal
   */
  public static final class Template extends hydra.javaScript.syntax.Literal implements Serializable {
    public final hydra.javaScript.syntax.TemplateLiteral value;

    public Template (hydra.javaScript.syntax.TemplateLiteral value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Template)) {
        return false;
      }
      Template o = (Template) other;
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
    public int compareTo(Literal other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Template o = (Template) other;
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
