// Note: this is an automatically generated file. Do not edit.

package hydra.errors;

import java.io.Serializable;

/**
 * An error that occurred while resolving a name, primitive, or record/union shape in a graph
 */
public abstract class ResolutionError implements Serializable, Comparable<ResolutionError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.errors.ResolutionError");

  public static final hydra.core.Name NO_SUCH_BINDING = new hydra.core.Name("noSuchBinding");

  public static final hydra.core.Name NO_SUCH_PRIMITIVE = new hydra.core.Name("noSuchPrimitive");

  public static final hydra.core.Name NO_MATCHING_FIELD = new hydra.core.Name("noMatchingField");

  public static final hydra.core.Name OTHER = new hydra.core.Name("other");

  public static final hydra.core.Name UNEXPECTED_SHAPE = new hydra.core.Name("unexpectedShape");

  private ResolutionError () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(NoSuchBinding instance) ;

    R visit(NoSuchPrimitive instance) ;

    R visit(NoMatchingField instance) ;

    R visit(Other instance) ;

    R visit(UnexpectedShape instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ResolutionError instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(NoSuchBinding instance) {
      return otherwise(instance);
    }

    default R visit(NoSuchPrimitive instance) {
      return otherwise(instance);
    }

    default R visit(NoMatchingField instance) {
      return otherwise(instance);
    }

    default R visit(Other instance) {
      return otherwise(instance);
    }

    default R visit(UnexpectedShape instance) {
      return otherwise(instance);
    }
  }

  /**
   * No binding with the expected name was found in the graph
   */
  public static final class NoSuchBinding extends hydra.errors.ResolutionError implements Serializable {
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
    public int compareTo(ResolutionError other) {
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
   * No primitive function with the expected name was found in the graph
   */
  public static final class NoSuchPrimitive extends hydra.errors.ResolutionError implements Serializable {
    public final hydra.errors.NoSuchPrimitiveError value;

    public NoSuchPrimitive (hydra.errors.NoSuchPrimitiveError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NoSuchPrimitive)) {
        return false;
      }
      NoSuchPrimitive o = (NoSuchPrimitive) other;
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
    public int compareTo(ResolutionError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NoSuchPrimitive o = (NoSuchPrimitive) other;
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
   * No field with the expected name was present in a record or case statement
   */
  public static final class NoMatchingField extends hydra.errors.ResolutionError implements Serializable {
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
    public int compareTo(ResolutionError other) {
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
   * A generic resolution error carrying a message
   */
  public static final class Other extends hydra.errors.ResolutionError implements Serializable {
    public final hydra.errors.OtherResolutionError value;

    public Other (hydra.errors.OtherResolutionError value) {
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
    public int compareTo(ResolutionError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Other o = (Other) other;
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
   * A term had a shape other than the one expected (e.g. a record, an injection)
   */
  public static final class UnexpectedShape extends hydra.errors.ResolutionError implements Serializable {
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
    public int compareTo(ResolutionError other) {
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
