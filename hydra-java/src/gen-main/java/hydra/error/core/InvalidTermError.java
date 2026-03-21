// Note: this is an automatically generated file. Do not edit.

package hydra.error.core;

import java.io.Serializable;

/**
 * An error indicating that a term is invalid
 */
public abstract class InvalidTermError implements Serializable, Comparable<InvalidTermError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.core.InvalidTermError");

  public static final hydra.core.Name DUPLICATE_BINDING = new hydra.core.Name("duplicateBinding");

  public static final hydra.core.Name DUPLICATE_FIELD = new hydra.core.Name("duplicateField");

  private InvalidTermError () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(DuplicateBinding instance) ;

    R visit(DuplicateField instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InvalidTermError instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(DuplicateBinding instance) {
      return otherwise(instance);
    }

    default R visit(DuplicateField instance) {
      return otherwise(instance);
    }
  }

  /**
   * A duplicate binding name in a let expression
   */
  public static final class DuplicateBinding extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.DuplicateBindingError value;

    public DuplicateBinding (hydra.error.core.DuplicateBindingError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DuplicateBinding)) {
        return false;
      }
      DuplicateBinding o = (DuplicateBinding) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DuplicateBinding o = (DuplicateBinding) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A duplicate field name in a record or union type
   */
  public static final class DuplicateField extends hydra.error.core.InvalidTermError implements Serializable {
    public final hydra.error.core.DuplicateFieldError value;

    public DuplicateField (hydra.error.core.DuplicateFieldError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DuplicateField)) {
        return false;
      }
      DuplicateField o = (DuplicateField) other;
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
    public int compareTo(InvalidTermError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DuplicateField o = (DuplicateField) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
