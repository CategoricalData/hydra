// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * Qualification for Require/Import commands
 */
public abstract class ImportQualification implements Serializable, Comparable<ImportQualification> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.ImportQualification");

  public static final hydra.core.Name IMPORT = new hydra.core.Name("import");

  public static final hydra.core.Name EXPORT = new hydra.core.Name("export");

  private ImportQualification () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Import instance) ;

    R visit(Export instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ImportQualification instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Import instance) {
      return otherwise(instance);
    }

    default R visit(Export instance) {
      return otherwise(instance);
    }
  }

  public static final class Import extends hydra.coq.syntax.ImportQualification implements Serializable {
    public Import () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Import)) {
        return false;
      }
      Import o = (Import) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ImportQualification other) {
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

  public static final class Export extends hydra.coq.syntax.ImportQualification implements Serializable {
    public Export () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Export)) {
        return false;
      }
      Export o = (Export) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ImportQualification other) {
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
