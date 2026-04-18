// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * A raw S-expression. This is an escape hatch for expressing arbitrary Lisp forms that do not fit into the structured AST above.
 */
public abstract class SExpression implements Serializable, Comparable<SExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.SExpression");

  public static final hydra.core.Name ATOM = new hydra.core.Name("atom");

  public static final hydra.core.Name LIST = new hydra.core.Name("list");

  private SExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Atom instance) ;

    R visit(List instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Atom instance) {
      return otherwise(instance);
    }

    default R visit(List instance) {
      return otherwise(instance);
    }
  }

  /**
   * An atomic value
   */
  public static final class Atom extends hydra.lisp.syntax.SExpression implements Serializable {
    public final String value;

    public Atom (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Atom)) {
        return false;
      }
      Atom o = (Atom) other;
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
    public int compareTo(SExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Atom o = (Atom) other;
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
   * A list of S-expressions
   */
  public static final class List extends hydra.lisp.syntax.SExpression implements Serializable {
    public final java.util.List<hydra.lisp.syntax.SExpression> value;

    public List (java.util.List<hydra.lisp.syntax.SExpression> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) other;
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
    public int compareTo(SExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      List o = (List) other;
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
