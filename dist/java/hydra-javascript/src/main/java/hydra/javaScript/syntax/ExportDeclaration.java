// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * An export declaration
 */
public abstract class ExportDeclaration implements Serializable, Comparable<ExportDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ExportDeclaration");

  public static final hydra.core.Name NAMED = new hydra.core.Name("named");

  public static final hydra.core.Name DEFAULT = new hydra.core.Name("default");

  public static final hydra.core.Name DECLARATION = new hydra.core.Name("declaration");

  public static final hydra.core.Name ALL = new hydra.core.Name("all");

  private ExportDeclaration () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Named instance) ;

    R visit(Default instance) ;

    R visit(Declaration instance) ;

    R visit(All instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ExportDeclaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Named instance) {
      return otherwise(instance);
    }

    default R visit(Default instance) {
      return otherwise(instance);
    }

    default R visit(Declaration instance) {
      return otherwise(instance);
    }

    default R visit(All instance) {
      return otherwise(instance);
    }
  }

  /**
   * Named exports (export {x, y as z})
   */
  public static final class Named extends hydra.javaScript.syntax.ExportDeclaration implements Serializable {
    public final hydra.javaScript.syntax.NamedExport value;

    public Named (hydra.javaScript.syntax.NamedExport value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Named)) {
        return false;
      }
      Named o = (Named) other;
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
    public int compareTo(ExportDeclaration other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Named o = (Named) other;
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
   * Default export (export default ...)
   */
  public static final class Default extends hydra.javaScript.syntax.ExportDeclaration implements Serializable {
    public final hydra.javaScript.syntax.Expression value;

    public Default (hydra.javaScript.syntax.Expression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Default)) {
        return false;
      }
      Default o = (Default) other;
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
    public int compareTo(ExportDeclaration other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Default o = (Default) other;
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
   * Export a declaration (export const x = ...)
   */
  public static final class Declaration extends hydra.javaScript.syntax.ExportDeclaration implements Serializable {
    public final hydra.javaScript.syntax.Statement value;

    public Declaration (hydra.javaScript.syntax.Statement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Declaration)) {
        return false;
      }
      Declaration o = (Declaration) other;
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
    public int compareTo(ExportDeclaration other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Declaration o = (Declaration) other;
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
   * Export all (export * from ...)
   */
  public static final class All extends hydra.javaScript.syntax.ExportDeclaration implements Serializable {
    public final hydra.javaScript.syntax.ExportAllDeclaration value;

    public All (hydra.javaScript.syntax.ExportAllDeclaration value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof All)) {
        return false;
      }
      All o = (All) other;
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
    public int compareTo(ExportDeclaration other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      All o = (All) other;
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
