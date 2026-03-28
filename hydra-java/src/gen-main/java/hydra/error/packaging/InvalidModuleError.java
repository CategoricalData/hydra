// Note: this is an automatically generated file. Do not edit.

package hydra.error.packaging;

import java.io.Serializable;

/**
 * An error indicating that a module is invalid
 */
public abstract class InvalidModuleError implements Serializable, Comparable<InvalidModuleError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.packaging.InvalidModuleError");

  public static final hydra.core.Name CONFLICTING_VARIANT_NAME = new hydra.core.Name("conflictingVariantName");

  public static final hydra.core.Name DEFINITION_NOT_IN_MODULE_NAMESPACE = new hydra.core.Name("definitionNotInModuleNamespace");

  public static final hydra.core.Name DUPLICATE_DEFINITION_NAME = new hydra.core.Name("duplicateDefinitionName");

  private InvalidModuleError () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ConflictingVariantName instance) ;

    R visit(DefinitionNotInModuleNamespace instance) ;

    R visit(DuplicateDefinitionName instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InvalidModuleError instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ConflictingVariantName instance) {
      return otherwise(instance);
    }

    default R visit(DefinitionNotInModuleNamespace instance) {
      return otherwise(instance);
    }

    default R visit(DuplicateDefinitionName instance) {
      return otherwise(instance);
    }
  }

  /**
   * A union variant name that conflicts with another type definition when mapped to a target language
   */
  public static final class ConflictingVariantName extends hydra.error.packaging.InvalidModuleError implements Serializable {
    public final hydra.error.packaging.ConflictingVariantNameError value;

    public ConflictingVariantName (hydra.error.packaging.ConflictingVariantNameError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ConflictingVariantName)) {
        return false;
      }
      ConflictingVariantName o = (ConflictingVariantName) other;
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
    public int compareTo(InvalidModuleError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ConflictingVariantName o = (ConflictingVariantName) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A definition whose name does not have the module's namespace as a prefix
   */
  public static final class DefinitionNotInModuleNamespace extends hydra.error.packaging.InvalidModuleError implements Serializable {
    public final hydra.error.packaging.DefinitionNotInModuleNamespaceError value;

    public DefinitionNotInModuleNamespace (hydra.error.packaging.DefinitionNotInModuleNamespaceError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DefinitionNotInModuleNamespace)) {
        return false;
      }
      DefinitionNotInModuleNamespace o = (DefinitionNotInModuleNamespace) other;
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
    public int compareTo(InvalidModuleError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DefinitionNotInModuleNamespace o = (DefinitionNotInModuleNamespace) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * Two or more definitions in the same module share the same name
   */
  public static final class DuplicateDefinitionName extends hydra.error.packaging.InvalidModuleError implements Serializable {
    public final hydra.error.packaging.DuplicateDefinitionNameError value;

    public DuplicateDefinitionName (hydra.error.packaging.DuplicateDefinitionNameError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DuplicateDefinitionName)) {
        return false;
      }
      DuplicateDefinitionName o = (DuplicateDefinitionName) other;
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
    public int compareTo(InvalidModuleError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DuplicateDefinitionName o = (DuplicateDefinitionName) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
