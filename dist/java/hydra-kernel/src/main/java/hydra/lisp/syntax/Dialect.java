// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * A Lisp dialect
 */
public abstract class Dialect implements Serializable, Comparable<Dialect> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.Dialect");

  public static final hydra.core.Name CLOJURE = new hydra.core.Name("clojure");

  public static final hydra.core.Name EMACS_LISP = new hydra.core.Name("emacsLisp");

  public static final hydra.core.Name COMMON_LISP = new hydra.core.Name("commonLisp");

  public static final hydra.core.Name SCHEME = new hydra.core.Name("scheme");

  private Dialect () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Clojure instance) ;

    R visit(EmacsLisp instance) ;

    R visit(CommonLisp instance) ;

    R visit(Scheme instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Dialect instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Clojure instance) {
      return otherwise(instance);
    }

    default R visit(EmacsLisp instance) {
      return otherwise(instance);
    }

    default R visit(CommonLisp instance) {
      return otherwise(instance);
    }

    default R visit(Scheme instance) {
      return otherwise(instance);
    }
  }

  public static final class Clojure extends hydra.lisp.syntax.Dialect implements Serializable {
    public Clojure () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Clojure)) {
        return false;
      }
      Clojure o = (Clojure) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Dialect other) {
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

  public static final class EmacsLisp extends hydra.lisp.syntax.Dialect implements Serializable {
    public EmacsLisp () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EmacsLisp)) {
        return false;
      }
      EmacsLisp o = (EmacsLisp) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Dialect other) {
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

  public static final class CommonLisp extends hydra.lisp.syntax.Dialect implements Serializable {
    public CommonLisp () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CommonLisp)) {
        return false;
      }
      CommonLisp o = (CommonLisp) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Dialect other) {
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

  public static final class Scheme extends hydra.lisp.syntax.Dialect implements Serializable {
    public Scheme () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Scheme)) {
        return false;
      }
      Scheme o = (Scheme) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Dialect other) {
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
