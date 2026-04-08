// Note: this is an automatically generated file. Do not edit.

package hydra.errors;

import java.io.Serializable;

/**
 * An error that occurred during type inference
 */
public abstract class InferenceError implements Serializable, Comparable<InferenceError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.errors.InferenceError");

  public static final hydra.core.Name CHECKING = new hydra.core.Name("checking");

  public static final hydra.core.Name OTHER = new hydra.core.Name("other");

  public static final hydra.core.Name UNIFICATION = new hydra.core.Name("unification");

  private InferenceError () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Checking instance) ;

    R visit(Other instance) ;

    R visit(Unification instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InferenceError instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Checking instance) {
      return otherwise(instance);
    }

    default R visit(Other instance) {
      return otherwise(instance);
    }

    default R visit(Unification instance) {
      return otherwise(instance);
    }
  }

  /**
   * A type checking error encountered during inference
   */
  public static final class Checking extends hydra.errors.InferenceError implements Serializable {
    public final hydra.error.checking.CheckingError value;

    public Checking (hydra.error.checking.CheckingError value) {
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
    public int compareTo(InferenceError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Checking o = (Checking) other;
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
   * A generic inference error carrying a message and a subterm path. Placeholder arm; sites should migrate to typed variants.
   */
  public static final class Other extends hydra.errors.InferenceError implements Serializable {
    public final hydra.errors.OtherInferenceError value;

    public Other (hydra.errors.OtherInferenceError value) {
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
    public int compareTo(InferenceError other) {
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
   * A unification failure encountered while inferring types
   */
  public static final class Unification extends hydra.errors.InferenceError implements Serializable {
    public final hydra.errors.UnificationInferenceError value;

    public Unification (hydra.errors.UnificationInferenceError value) {
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
    public int compareTo(InferenceError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Unification o = (Unification) other;
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
