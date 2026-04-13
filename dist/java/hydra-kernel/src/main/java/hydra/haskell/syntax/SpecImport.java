// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * An import specification
 */
public abstract class SpecImport implements Serializable, Comparable<SpecImport> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.SpecImport");

  public static final hydra.core.Name LIST = new hydra.core.Name("list");

  public static final hydra.core.Name HIDING = new hydra.core.Name("hiding");

  private SpecImport () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(List instance) ;

    R visit(Hiding instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SpecImport instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(List instance) {
      return otherwise(instance);
    }

    default R visit(Hiding instance) {
      return otherwise(instance);
    }
  }

  /**
   * A list of imports to include
   */
  public static final class List extends hydra.haskell.syntax.SpecImport implements Serializable {
    public final java.util.List<hydra.haskell.syntax.ImportExportSpec> value;

    public List (java.util.List<hydra.haskell.syntax.ImportExportSpec> value) {
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
    public int compareTo(SpecImport other) {
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

  /**
   * A list of imports to exclude
   */
  public static final class Hiding extends hydra.haskell.syntax.SpecImport implements Serializable {
    public final java.util.List<hydra.haskell.syntax.ImportExportSpec> value;

    public Hiding (java.util.List<hydra.haskell.syntax.ImportExportSpec> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Hiding)) {
        return false;
      }
      Hiding o = (Hiding) other;
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
    public int compareTo(SpecImport other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Hiding o = (Hiding) other;
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
