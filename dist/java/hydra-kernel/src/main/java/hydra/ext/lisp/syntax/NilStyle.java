// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * The style of nil/null in a dialect
 */
public abstract class NilStyle implements Serializable, Comparable<NilStyle> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.NilStyle");

  public static final hydra.core.Name NIL = new hydra.core.Name("nil");

  public static final hydra.core.Name EMPTY_LIST = new hydra.core.Name("emptyList");

  private NilStyle () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Nil instance) ;

    R visit(EmptyList instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NilStyle instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Nil instance) {
      return otherwise(instance);
    }

    default R visit(EmptyList instance) {
      return otherwise(instance);
    }
  }

  public static final class Nil extends hydra.ext.lisp.syntax.NilStyle implements Serializable {
    public Nil () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nil)) {
        return false;
      }
      Nil o = (Nil) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(NilStyle other) {
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

  public static final class EmptyList extends hydra.ext.lisp.syntax.NilStyle implements Serializable {
    public EmptyList () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EmptyList)) {
        return false;
      }
      EmptyList o = (EmptyList) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(NilStyle other) {
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
