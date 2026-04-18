// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A top-level item in a module
 */
public abstract class ModuleItem implements Serializable, Comparable<ModuleItem> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ModuleItem");

  public static final hydra.core.Name STATEMENT = new hydra.core.Name("statement");

  public static final hydra.core.Name IMPORT = new hydra.core.Name("import");

  public static final hydra.core.Name EXPORT = new hydra.core.Name("export");

  private ModuleItem () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Statement instance) ;

    R visit(Import instance) ;

    R visit(Export instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ModuleItem instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Statement instance) {
      return otherwise(instance);
    }

    default R visit(Import instance) {
      return otherwise(instance);
    }

    default R visit(Export instance) {
      return otherwise(instance);
    }
  }

  public static final class Statement extends hydra.javaScript.syntax.ModuleItem implements Serializable {
    public final hydra.javaScript.syntax.Statement value;

    public Statement (hydra.javaScript.syntax.Statement value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Statement)) {
        return false;
      }
      Statement o = (Statement) other;
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
    public int compareTo(ModuleItem other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Statement o = (Statement) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Import extends hydra.javaScript.syntax.ModuleItem implements Serializable {
    public final hydra.javaScript.syntax.ImportDeclaration value;

    public Import (hydra.javaScript.syntax.ImportDeclaration value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Import)) {
        return false;
      }
      Import o = (Import) other;
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
    public int compareTo(ModuleItem other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Import o = (Import) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Export extends hydra.javaScript.syntax.ModuleItem implements Serializable {
    public final hydra.javaScript.syntax.ExportDeclaration value;

    public Export (hydra.javaScript.syntax.ExportDeclaration value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Export)) {
        return false;
      }
      Export o = (Export) other;
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
    public int compareTo(ModuleItem other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Export o = (Export) other;
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
