// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A unary operator
 */
public abstract class UnaryOperator implements Serializable, Comparable<UnaryOperator> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.UnaryOperator");

  public static final hydra.core.Name NEGATE = new hydra.core.Name("negate");

  public static final hydra.core.Name PLUS = new hydra.core.Name("plus");

  public static final hydra.core.Name NOT = new hydra.core.Name("not");

  public static final hydra.core.Name BITWISE_NOT = new hydra.core.Name("bitwiseNot");

  public static final hydra.core.Name TYPEOF = new hydra.core.Name("typeof");

  public static final hydra.core.Name VOID = new hydra.core.Name("void");

  public static final hydra.core.Name DELETE = new hydra.core.Name("delete");

  public static final hydra.core.Name INCREMENT = new hydra.core.Name("increment");

  public static final hydra.core.Name DECREMENT = new hydra.core.Name("decrement");

  private UnaryOperator () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Negate instance) ;

    R visit(Plus instance) ;

    R visit(Not instance) ;

    R visit(BitwiseNot instance) ;

    R visit(Typeof instance) ;

    R visit(Void_ instance) ;

    R visit(Delete instance) ;

    R visit(Increment instance) ;

    R visit(Decrement instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UnaryOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Negate instance) {
      return otherwise(instance);
    }

    default R visit(Plus instance) {
      return otherwise(instance);
    }

    default R visit(Not instance) {
      return otherwise(instance);
    }

    default R visit(BitwiseNot instance) {
      return otherwise(instance);
    }

    default R visit(Typeof instance) {
      return otherwise(instance);
    }

    default R visit(Void_ instance) {
      return otherwise(instance);
    }

    default R visit(Delete instance) {
      return otherwise(instance);
    }

    default R visit(Increment instance) {
      return otherwise(instance);
    }

    default R visit(Decrement instance) {
      return otherwise(instance);
    }
  }

  /**
   * -
   */
  public static final class Negate extends hydra.javaScript.syntax.UnaryOperator implements Serializable {
    public Negate () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Negate)) {
        return false;
      }
      Negate o = (Negate) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(UnaryOperator other) {
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
   * +
   */
  public static final class Plus extends hydra.javaScript.syntax.UnaryOperator implements Serializable {
    public Plus () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Plus)) {
        return false;
      }
      Plus o = (Plus) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(UnaryOperator other) {
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
   * !
   */
  public static final class Not extends hydra.javaScript.syntax.UnaryOperator implements Serializable {
    public Not () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Not)) {
        return false;
      }
      Not o = (Not) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(UnaryOperator other) {
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
   * ~
   */
  public static final class BitwiseNot extends hydra.javaScript.syntax.UnaryOperator implements Serializable {
    public BitwiseNot () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BitwiseNot)) {
        return false;
      }
      BitwiseNot o = (BitwiseNot) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(UnaryOperator other) {
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
   * typeof
   */
  public static final class Typeof extends hydra.javaScript.syntax.UnaryOperator implements Serializable {
    public Typeof () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Typeof)) {
        return false;
      }
      Typeof o = (Typeof) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(UnaryOperator other) {
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
   * void
   */
  public static final class Void_ extends hydra.javaScript.syntax.UnaryOperator implements Serializable {
    public Void_ () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Void_)) {
        return false;
      }
      Void_ o = (Void_) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(UnaryOperator other) {
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
   * delete
   */
  public static final class Delete extends hydra.javaScript.syntax.UnaryOperator implements Serializable {
    public Delete () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Delete)) {
        return false;
      }
      Delete o = (Delete) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(UnaryOperator other) {
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
   * ++
   */
  public static final class Increment extends hydra.javaScript.syntax.UnaryOperator implements Serializable {
    public Increment () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Increment)) {
        return false;
      }
      Increment o = (Increment) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(UnaryOperator other) {
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
   * --
   */
  public static final class Decrement extends hydra.javaScript.syntax.UnaryOperator implements Serializable {
    public Decrement () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Decrement)) {
        return false;
      }
      Decrement o = (Decrement) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(UnaryOperator other) {
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
