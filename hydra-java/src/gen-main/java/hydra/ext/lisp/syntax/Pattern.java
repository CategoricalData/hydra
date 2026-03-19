// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A pattern for use in case expressions or match forms
 */
public abstract class Pattern implements Serializable, Comparable<Pattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.Pattern");

  public static final hydra.core.Name CONSTRUCTOR = new hydra.core.Name("constructor");

  public static final hydra.core.Name LITERAL = new hydra.core.Name("literal");

  public static final hydra.core.Name WILDCARD = new hydra.core.Name("wildcard");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  private Pattern () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Constructor instance) ;

    R visit(Literal instance) ;

    R visit(Wildcard instance) ;

    R visit(Variable instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Pattern instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Constructor instance) {
      return otherwise(instance);
    }

    default R visit(Literal instance) {
      return otherwise(instance);
    }

    default R visit(Wildcard instance) {
      return otherwise(instance);
    }

    default R visit(Variable instance) {
      return otherwise(instance);
    }
  }

  /**
   * A constructor pattern (for union/sum type matching)
   */
  public static final class Constructor extends hydra.ext.lisp.syntax.Pattern implements Serializable {
    public final hydra.ext.lisp.syntax.ConstructorPattern value;

    public Constructor (hydra.ext.lisp.syntax.ConstructorPattern value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Constructor)) {
        return false;
      }
      Constructor o = (Constructor) other;
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
    public int compareTo(Pattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Constructor o = (Constructor) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A literal pattern
   */
  public static final class Literal extends hydra.ext.lisp.syntax.Pattern implements Serializable {
    public final hydra.ext.lisp.syntax.LiteralPattern value;

    public Literal (hydra.ext.lisp.syntax.LiteralPattern value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) other;
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
    public int compareTo(Pattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Literal o = (Literal) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A wildcard pattern matching anything
   */
  public static final class Wildcard extends hydra.ext.lisp.syntax.Pattern implements Serializable {
    public final hydra.ext.lisp.syntax.WildcardPattern value;

    public Wildcard (hydra.ext.lisp.syntax.WildcardPattern value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Wildcard)) {
        return false;
      }
      Wildcard o = (Wildcard) other;
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
    public int compareTo(Pattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Wildcard o = (Wildcard) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A variable pattern that binds the matched value
   */
  public static final class Variable extends hydra.ext.lisp.syntax.Pattern implements Serializable {
    public final hydra.ext.lisp.syntax.Symbol value;

    public Variable (hydra.ext.lisp.syntax.Symbol value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variable)) {
        return false;
      }
      Variable o = (Variable) other;
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
    public int compareTo(Pattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Variable o = (Variable) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
