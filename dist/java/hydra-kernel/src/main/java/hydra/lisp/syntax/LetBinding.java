// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * A single binding in a let expression
 */
public abstract class LetBinding implements Serializable, Comparable<LetBinding> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.LetBinding");

  public static final hydra.core.Name SIMPLE = new hydra.core.Name("simple");

  public static final hydra.core.Name DESTRUCTURING = new hydra.core.Name("destructuring");

  private LetBinding () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Simple instance) ;

    R visit(Destructuring instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LetBinding instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Simple instance) {
      return otherwise(instance);
    }

    default R visit(Destructuring instance) {
      return otherwise(instance);
    }
  }

  /**
   * A simple name-value binding
   */
  public static final class Simple extends hydra.lisp.syntax.LetBinding implements Serializable {
    public final hydra.lisp.syntax.SimpleBinding value;

    public Simple (hydra.lisp.syntax.SimpleBinding value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) other;
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
    public int compareTo(LetBinding other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Simple o = (Simple) other;
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
   * A destructuring binding
   */
  public static final class Destructuring extends hydra.lisp.syntax.LetBinding implements Serializable {
    public final hydra.lisp.syntax.DestructuringBinding value;

    public Destructuring (hydra.lisp.syntax.DestructuringBinding value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Destructuring)) {
        return false;
      }
      Destructuring o = (Destructuring) other;
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
    public int compareTo(LetBinding other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Destructuring o = (Destructuring) other;
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
