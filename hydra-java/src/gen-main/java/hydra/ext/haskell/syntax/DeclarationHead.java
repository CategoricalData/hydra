// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.syntax;

import java.io.Serializable;

/**
 * The left-hand side of a declaration
 */
public abstract class DeclarationHead implements Serializable, Comparable<DeclarationHead> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.haskell.syntax.DeclarationHead");

  public static final hydra.core.Name APPLICATION = new hydra.core.Name("application");

  public static final hydra.core.Name PARENS = new hydra.core.Name("parens");

  public static final hydra.core.Name SIMPLE = new hydra.core.Name("simple");

  private DeclarationHead () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Application instance) ;

    R visit(Parens instance) ;

    R visit(Simple instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DeclarationHead instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Application instance) {
      return otherwise(instance);
    }

    default R visit(Parens instance) {
      return otherwise(instance);
    }

    default R visit(Simple instance) {
      return otherwise(instance);
    }
  }

  /**
   * An application-style declaration head
   */
  public static final class Application extends hydra.ext.haskell.syntax.DeclarationHead implements Serializable {
    public final hydra.ext.haskell.syntax.ApplicationDeclarationHead value;

    public Application (hydra.ext.haskell.syntax.ApplicationDeclarationHead value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Application)) {
        return false;
      }
      Application o = (Application) other;
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
    public int compareTo(DeclarationHead other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Application o = (Application) other;
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
   * A parenthesized declaration head
   */
  public static final class Parens extends hydra.ext.haskell.syntax.DeclarationHead implements Serializable {
    public final hydra.ext.haskell.syntax.DeclarationHead value;

    public Parens (hydra.ext.haskell.syntax.DeclarationHead value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parens)) {
        return false;
      }
      Parens o = (Parens) other;
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
    public int compareTo(DeclarationHead other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Parens o = (Parens) other;
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
   * A simple name
   */
  public static final class Simple extends hydra.ext.haskell.syntax.DeclarationHead implements Serializable {
    public final hydra.ext.haskell.syntax.Name value;

    public Simple (hydra.ext.haskell.syntax.Name value) {
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
    public int compareTo(DeclarationHead other) {
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
}
