// Note: this is an automatically generated file. Do not edit.

package hydra.error;

import java.io.Serializable;

/**
 * An error of any kind, with kernel errors particularly differentiated
 */
public abstract class Error_ implements Serializable, Comparable<Error_> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.Error");

  public static final hydra.core.Name CHECKING = new hydra.core.Name("checking");

  public static final hydra.core.Name DECODING = new hydra.core.Name("decoding");

  public static final hydra.core.Name DUPLICATE_BINDING = new hydra.core.Name("duplicateBinding");

  public static final hydra.core.Name DUPLICATE_FIELD = new hydra.core.Name("duplicateField");

  public static final hydra.core.Name OTHER = new hydra.core.Name("other");

  public static final hydra.core.Name UNDEFINED_FIELD = new hydra.core.Name("undefinedField");

  public static final hydra.core.Name UNDEFINED_TERM = new hydra.core.Name("undefinedTerm");

  public static final hydra.core.Name UNDEFINED_TYPE = new hydra.core.Name("undefinedType");

  public static final hydra.core.Name UNEXPECTED_TERM_VARIANT = new hydra.core.Name("unexpectedTermVariant");

  public static final hydra.core.Name UNEXPECTED_TYPE_VARIANT = new hydra.core.Name("unexpectedTypeVariant");

  public static final hydra.core.Name UNIFICATION = new hydra.core.Name("unification");

  private Error_ () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Checking instance) ;

    R visit(Decoding instance) ;

    R visit(DuplicateBinding instance) ;

    R visit(DuplicateField instance) ;

    R visit(Other instance) ;

    R visit(UndefinedField instance) ;

    R visit(UndefinedTerm instance) ;

    R visit(UndefinedType instance) ;

    R visit(UnexpectedTermVariant instance) ;

    R visit(UnexpectedTypeVariant instance) ;

    R visit(Unification instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Error_ instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Checking instance) {
      return otherwise(instance);
    }

    default R visit(Decoding instance) {
      return otherwise(instance);
    }

    default R visit(DuplicateBinding instance) {
      return otherwise(instance);
    }

    default R visit(DuplicateField instance) {
      return otherwise(instance);
    }

    default R visit(Other instance) {
      return otherwise(instance);
    }

    default R visit(UndefinedField instance) {
      return otherwise(instance);
    }

    default R visit(UndefinedTerm instance) {
      return otherwise(instance);
    }

    default R visit(UndefinedType instance) {
      return otherwise(instance);
    }

    default R visit(UnexpectedTermVariant instance) {
      return otherwise(instance);
    }

    default R visit(UnexpectedTypeVariant instance) {
      return otherwise(instance);
    }

    default R visit(Unification instance) {
      return otherwise(instance);
    }
  }

  /**
   * A type checking error
   */
  public static final class Checking extends hydra.error.Error_ implements Serializable {
    public final hydra.error.CheckingError value;

    public Checking (hydra.error.CheckingError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Checking)) {
        return false;
      }
      Checking o = (Checking) other;
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
    public int compareTo(Error_ other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Checking o = (Checking) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * An error that occurred during decoding of a term
   */
  public static final class Decoding extends hydra.error.Error_ implements Serializable {
    public final hydra.error.DecodingError value;

    public Decoding (hydra.error.DecodingError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Decoding)) {
        return false;
      }
      Decoding o = (Decoding) other;
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
    public int compareTo(Error_ other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Decoding o = (Decoding) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A duplicate binding name error
   */
  public static final class DuplicateBinding extends hydra.error.Error_ implements Serializable {
    public final hydra.error.DuplicateBindingError value;

    public DuplicateBinding (hydra.error.DuplicateBindingError value) {
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
    public int compareTo(Error_ other) {
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
   * A duplicate field name error
   */
  public static final class DuplicateField extends hydra.error.Error_ implements Serializable {
    public final hydra.error.DuplicateFieldError value;

    public DuplicateField (hydra.error.DuplicateFieldError value) {
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
    public int compareTo(Error_ other) {
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

  /**
   * Any other error
   */
  public static final class Other extends hydra.error.Error_ implements Serializable {
    public final hydra.error.OtherError value;

    public Other (hydra.error.OtherError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Other)) {
        return false;
      }
      Other o = (Other) other;
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
    public int compareTo(Error_ other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Other o = (Other) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A reference to an undefined field
   */
  public static final class UndefinedField extends hydra.error.Error_ implements Serializable {
    public final hydra.error.UndefinedFieldError value;

    public UndefinedField (hydra.error.UndefinedFieldError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UndefinedField)) {
        return false;
      }
      UndefinedField o = (UndefinedField) other;
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
    public int compareTo(Error_ other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UndefinedField o = (UndefinedField) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A reference to an undefined term
   */
  public static final class UndefinedTerm extends hydra.error.Error_ implements Serializable {
    public final hydra.error.UndefinedTermError value;

    public UndefinedTerm (hydra.error.UndefinedTermError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UndefinedTerm)) {
        return false;
      }
      UndefinedTerm o = (UndefinedTerm) other;
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
    public int compareTo(Error_ other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UndefinedTerm o = (UndefinedTerm) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A reference to an undefined type
   */
  public static final class UndefinedType extends hydra.error.Error_ implements Serializable {
    public final hydra.error.UndefinedTypeError value;

    public UndefinedType (hydra.error.UndefinedTypeError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UndefinedType)) {
        return false;
      }
      UndefinedType o = (UndefinedType) other;
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
    public int compareTo(Error_ other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UndefinedType o = (UndefinedType) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * An unexpected term variant
   */
  public static final class UnexpectedTermVariant extends hydra.error.Error_ implements Serializable {
    public final hydra.error.UnexpectedTermVariantError value;

    public UnexpectedTermVariant (hydra.error.UnexpectedTermVariantError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnexpectedTermVariant)) {
        return false;
      }
      UnexpectedTermVariant o = (UnexpectedTermVariant) other;
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
    public int compareTo(Error_ other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UnexpectedTermVariant o = (UnexpectedTermVariant) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * An unexpected type variant
   */
  public static final class UnexpectedTypeVariant extends hydra.error.Error_ implements Serializable {
    public final hydra.error.UnexpectedTypeVariantError value;

    public UnexpectedTypeVariant (hydra.error.UnexpectedTypeVariantError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnexpectedTypeVariant)) {
        return false;
      }
      UnexpectedTypeVariant o = (UnexpectedTypeVariant) other;
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
    public int compareTo(Error_ other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UnexpectedTypeVariant o = (UnexpectedTypeVariant) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A type unification error
   */
  public static final class Unification extends hydra.error.Error_ implements Serializable {
    public final hydra.error.UnificationError value;

    public Unification (hydra.error.UnificationError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unification)) {
        return false;
      }
      Unification o = (Unification) other;
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
    public int compareTo(Error_ other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Unification o = (Unification) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
