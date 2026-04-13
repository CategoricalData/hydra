// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * The kind of let binding
 */
public abstract class LetKind implements Serializable, Comparable<LetKind> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.LetKind");

  public static final hydra.core.Name PARALLEL = new hydra.core.Name("parallel");

  public static final hydra.core.Name SEQUENTIAL = new hydra.core.Name("sequential");

  public static final hydra.core.Name RECURSIVE = new hydra.core.Name("recursive");

  private LetKind () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Parallel instance) ;

    R visit(Sequential instance) ;

    R visit(Recursive instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LetKind instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Parallel instance) {
      return otherwise(instance);
    }

    default R visit(Sequential instance) {
      return otherwise(instance);
    }

    default R visit(Recursive instance) {
      return otherwise(instance);
    }
  }

  public static final class Parallel extends hydra.lisp.syntax.LetKind implements Serializable {
    public Parallel () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parallel)) {
        return false;
      }
      Parallel o = (Parallel) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(LetKind other) {
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

  public static final class Sequential extends hydra.lisp.syntax.LetKind implements Serializable {
    public Sequential () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequential)) {
        return false;
      }
      Sequential o = (Sequential) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(LetKind other) {
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

  public static final class Recursive extends hydra.lisp.syntax.LetKind implements Serializable {
    public Recursive () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Recursive)) {
        return false;
      }
      Recursive o = (Recursive) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(LetKind other) {
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
