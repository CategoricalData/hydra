// Note: this is an automatically generated file. Do not edit.

package hydra.errors;

import java.io.Serializable;

/**
 * An error that occurred while extracting a typed value from a term
 */
public abstract class ExtractionError implements Serializable, Comparable<ExtractionError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.errors.ExtractionError");

  public static final hydra.core.Name EMPTY_LIST = new hydra.core.Name("emptyList");

  public static final hydra.core.Name MULTIPLE_BINDINGS = new hydra.core.Name("multipleBindings");

  public static final hydra.core.Name MULTIPLE_FIELDS = new hydra.core.Name("multipleFields");

  public static final hydra.core.Name NO_MATCHING_FIELD = new hydra.core.Name("noMatchingField");

  public static final hydra.core.Name NO_SUCH_BINDING = new hydra.core.Name("noSuchBinding");

  public static final hydra.core.Name NOT_ENOUGH_CASES = new hydra.core.Name("notEnoughCases");

  public static final hydra.core.Name UNEXPECTED_SHAPE = new hydra.core.Name("unexpectedShape");

  private ExtractionError () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(EmptyList instance) ;

    R visit(MultipleBindings instance) ;

    R visit(MultipleFields instance) ;

    R visit(NoMatchingField instance) ;

    R visit(NoSuchBinding instance) ;

    R visit(NotEnoughCases instance) ;

    R visit(UnexpectedShape instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ExtractionError instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(EmptyList instance) {
      return otherwise(instance);
    }

    default R visit(MultipleBindings instance) {
      return otherwise(instance);
    }

    default R visit(MultipleFields instance) {
      return otherwise(instance);
    }

    default R visit(NoMatchingField instance) {
      return otherwise(instance);
    }

    default R visit(NoSuchBinding instance) {
      return otherwise(instance);
    }

    default R visit(NotEnoughCases instance) {
      return otherwise(instance);
    }

    default R visit(UnexpectedShape instance) {
      return otherwise(instance);
    }
  }

  /**
   * An empty list was encountered where a non-empty list was required
   */
  public static final class EmptyList extends hydra.errors.ExtractionError implements Serializable {
    public final java.lang.Void value;

    public EmptyList (java.lang.Void value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EmptyList)) {
        return false;
      }
      EmptyList o = (EmptyList) other;
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
    public int compareTo(ExtractionError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      EmptyList o = (EmptyList) other;
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
   * Multiple let bindings were found with the same name
   */
  public static final class MultipleBindings extends hydra.errors.ExtractionError implements Serializable {
    public final hydra.errors.MultipleBindingsError value;

    public MultipleBindings (hydra.errors.MultipleBindingsError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MultipleBindings)) {
        return false;
      }
      MultipleBindings o = (MultipleBindings) other;
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
    public int compareTo(ExtractionError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MultipleBindings o = (MultipleBindings) other;
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
   * Multiple record fields were found with the same field name
   */
  public static final class MultipleFields extends hydra.errors.ExtractionError implements Serializable {
    public final hydra.errors.MultipleFieldsError value;

    public MultipleFields (hydra.errors.MultipleFieldsError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MultipleFields)) {
        return false;
      }
      MultipleFields o = (MultipleFields) other;
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
    public int compareTo(ExtractionError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MultipleFields o = (MultipleFields) other;
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
   * No field with the expected name was found in a record
   */
  public static final class NoMatchingField extends hydra.errors.ExtractionError implements Serializable {
    public final hydra.errors.NoMatchingFieldError value;

    public NoMatchingField (hydra.errors.NoMatchingFieldError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NoMatchingField)) {
        return false;
      }
      NoMatchingField o = (NoMatchingField) other;
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
    public int compareTo(ExtractionError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NoMatchingField o = (NoMatchingField) other;
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
   * No let binding with the expected name was found
   */
  public static final class NoSuchBinding extends hydra.errors.ExtractionError implements Serializable {
    public final hydra.errors.NoSuchBindingError value;

    public NoSuchBinding (hydra.errors.NoSuchBindingError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NoSuchBinding)) {
        return false;
      }
      NoSuchBinding o = (NoSuchBinding) other;
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
    public int compareTo(ExtractionError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NoSuchBinding o = (NoSuchBinding) other;
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
   * A case statement did not contain enough cases to match the target
   */
  public static final class NotEnoughCases extends hydra.errors.ExtractionError implements Serializable {
    public final java.lang.Void value;

    public NotEnoughCases (java.lang.Void value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotEnoughCases)) {
        return false;
      }
      NotEnoughCases o = (NotEnoughCases) other;
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
    public int compareTo(ExtractionError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NotEnoughCases o = (NotEnoughCases) other;
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
   * A term, type, literal, or other value had an unexpected shape
   */
  public static final class UnexpectedShape extends hydra.errors.ExtractionError implements Serializable {
    public final hydra.errors.UnexpectedShapeError value;

    public UnexpectedShape (hydra.errors.UnexpectedShapeError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnexpectedShape)) {
        return false;
      }
      UnexpectedShape o = (UnexpectedShape) other;
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
    public int compareTo(ExtractionError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UnexpectedShape o = (UnexpectedShape) other;
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
