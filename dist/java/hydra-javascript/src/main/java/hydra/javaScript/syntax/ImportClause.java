// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * An import clause (named, default, or namespace import)
 */
public abstract class ImportClause implements Serializable, Comparable<ImportClause> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ImportClause");

  public static final hydra.core.Name NAMED = new hydra.core.Name("named");

  public static final hydra.core.Name DEFAULT = new hydra.core.Name("default");

  public static final hydra.core.Name NAMESPACE = new hydra.core.Name("namespace");

  private ImportClause () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Named instance) ;

    R visit(Default instance) ;

    R visit(Namespace instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ImportClause instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Named instance) {
      return otherwise(instance);
    }

    default R visit(Default instance) {
      return otherwise(instance);
    }

    default R visit(Namespace instance) {
      return otherwise(instance);
    }
  }

  public static final class Named extends hydra.javaScript.syntax.ImportClause implements Serializable {
    public final hydra.javaScript.syntax.ImportSpecifier value;

    public Named (hydra.javaScript.syntax.ImportSpecifier value) {
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
    public int compareTo(ImportClause other) {
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

  public static final class Default extends hydra.javaScript.syntax.ImportClause implements Serializable {
    public final hydra.javaScript.syntax.ImportDefaultSpecifier value;

    public Default (hydra.javaScript.syntax.ImportDefaultSpecifier value) {
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
    public int compareTo(ImportClause other) {
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

  public static final class Namespace extends hydra.javaScript.syntax.ImportClause implements Serializable {
    public final hydra.javaScript.syntax.ImportNamespaceSpecifier value;

    public Namespace (hydra.javaScript.syntax.ImportNamespaceSpecifier value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Namespace)) {
        return false;
      }
      Namespace o = (Namespace) other;
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
    public int compareTo(ImportClause other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Namespace o = (Namespace) other;
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
