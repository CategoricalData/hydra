// Note: this is an automatically generated file. Do not edit.

package hydra.error.packaging;

import java.io.Serializable;

/**
 * An error indicating that a package is invalid
 */
public abstract class InvalidPackageError implements Serializable, Comparable<InvalidPackageError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.packaging.InvalidPackageError");

  public static final hydra.core.Name CONFLICTING_MODULE_NAMESPACE = new hydra.core.Name("conflictingModuleNamespace");

  public static final hydra.core.Name DUPLICATE_MODULE_NAMESPACE = new hydra.core.Name("duplicateModuleNamespace");

  public static final hydra.core.Name INVALID_MODULE = new hydra.core.Name("invalidModule");

  private InvalidPackageError () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ConflictingModuleNamespace instance) ;

    R visit(DuplicateModuleNamespace instance) ;

    R visit(InvalidModule instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InvalidPackageError instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ConflictingModuleNamespace instance) {
      return otherwise(instance);
    }

    default R visit(DuplicateModuleNamespace instance) {
      return otherwise(instance);
    }

    default R visit(InvalidModule instance) {
      return otherwise(instance);
    }
  }

  /**
   * Two module namespaces that conflict when mapped to a target language
   */
  public static final class ConflictingModuleNamespace extends hydra.error.packaging.InvalidPackageError implements Serializable {
    public final hydra.error.packaging.ConflictingModuleNamespaceError value;

    public ConflictingModuleNamespace (hydra.error.packaging.ConflictingModuleNamespaceError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ConflictingModuleNamespace)) {
        return false;
      }
      ConflictingModuleNamespace o = (ConflictingModuleNamespace) other;
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
    public int compareTo(InvalidPackageError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ConflictingModuleNamespace o = (ConflictingModuleNamespace) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Two or more modules in the same package share the same namespace
   */
  public static final class DuplicateModuleNamespace extends hydra.error.packaging.InvalidPackageError implements Serializable {
    public final hydra.error.packaging.DuplicateModuleNamespaceError value;

    public DuplicateModuleNamespace (hydra.error.packaging.DuplicateModuleNamespaceError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DuplicateModuleNamespace)) {
        return false;
      }
      DuplicateModuleNamespace o = (DuplicateModuleNamespace) other;
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
    public int compareTo(InvalidPackageError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DuplicateModuleNamespace o = (DuplicateModuleNamespace) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A module within the package is invalid
   */
  public static final class InvalidModule extends hydra.error.packaging.InvalidPackageError implements Serializable {
    public final hydra.error.packaging.InvalidModuleError value;

    public InvalidModule (hydra.error.packaging.InvalidModuleError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InvalidModule)) {
        return false;
      }
      InvalidModule o = (InvalidModule) other;
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
    public int compareTo(InvalidPackageError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      InvalidModule o = (InvalidModule) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
