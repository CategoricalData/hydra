// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * The kind of a class method
 */
public abstract class MethodKind implements Serializable, Comparable<MethodKind> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.MethodKind");

  public static final hydra.core.Name CONSTRUCTOR = new hydra.core.Name("constructor");

  public static final hydra.core.Name METHOD = new hydra.core.Name("method");

  public static final hydra.core.Name GET = new hydra.core.Name("get");

  public static final hydra.core.Name SET = new hydra.core.Name("set");

  private MethodKind () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Constructor instance) ;

    R visit(Method instance) ;

    R visit(Get instance) ;

    R visit(Set instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(MethodKind instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Constructor instance) {
      return otherwise(instance);
    }

    default R visit(Method instance) {
      return otherwise(instance);
    }

    default R visit(Get instance) {
      return otherwise(instance);
    }

    default R visit(Set instance) {
      return otherwise(instance);
    }
  }

  public static final class Constructor extends hydra.javaScript.syntax.MethodKind implements Serializable {
    public Constructor () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Constructor)) {
        return false;
      }
      Constructor o = (Constructor) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(MethodKind other) {
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

  public static final class Method extends hydra.javaScript.syntax.MethodKind implements Serializable {
    public Method () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Method)) {
        return false;
      }
      Method o = (Method) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(MethodKind other) {
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

  public static final class Get extends hydra.javaScript.syntax.MethodKind implements Serializable {
    public Get () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Get)) {
        return false;
      }
      Get o = (Get) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(MethodKind other) {
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

  public static final class Set extends hydra.javaScript.syntax.MethodKind implements Serializable {
    public Set () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
        return false;
      }
      Set o = (Set) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(MethodKind other) {
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
