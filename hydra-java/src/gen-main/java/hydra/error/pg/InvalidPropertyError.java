// Note: this is an automatically generated file. Do not edit.

package hydra.error.pg;

import java.io.Serializable;

/**
 * An error indicating that a property is invalid
 */
public abstract class InvalidPropertyError implements Serializable, Comparable<InvalidPropertyError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.pg.InvalidPropertyError");

  public static final hydra.core.Name INVALID_VALUE = new hydra.core.Name("invalidValue");

  public static final hydra.core.Name MISSING_REQUIRED = new hydra.core.Name("missingRequired");

  public static final hydra.core.Name UNEXPECTED_KEY = new hydra.core.Name("unexpectedKey");

  private InvalidPropertyError () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(InvalidValue instance) ;

    R visit(MissingRequired instance) ;

    R visit(UnexpectedKey instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InvalidPropertyError instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(InvalidValue instance) {
      return otherwise(instance);
    }

    default R visit(MissingRequired instance) {
      return otherwise(instance);
    }

    default R visit(UnexpectedKey instance) {
      return otherwise(instance);
    }
  }

  /**
   * The property value failed type validation
   */
  public static final class InvalidValue extends hydra.error.pg.InvalidPropertyError implements Serializable {
    public final hydra.error.pg.InvalidValueError value;

    public InvalidValue (hydra.error.pg.InvalidValueError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InvalidValue)) {
        return false;
      }
      InvalidValue o = (InvalidValue) other;
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
    public int compareTo(InvalidPropertyError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      InvalidValue o = (InvalidValue) other;
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
   * A required property is missing
   */
  public static final class MissingRequired extends hydra.error.pg.InvalidPropertyError implements Serializable {
    public final hydra.pg.model.PropertyKey value;

    public MissingRequired (hydra.pg.model.PropertyKey value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MissingRequired)) {
        return false;
      }
      MissingRequired o = (MissingRequired) other;
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
    public int compareTo(InvalidPropertyError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MissingRequired o = (MissingRequired) other;
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
   * A property has an unexpected key not in the schema
   */
  public static final class UnexpectedKey extends hydra.error.pg.InvalidPropertyError implements Serializable {
    public final hydra.pg.model.PropertyKey value;

    public UnexpectedKey (hydra.pg.model.PropertyKey value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnexpectedKey)) {
        return false;
      }
      UnexpectedKey o = (UnexpectedKey) other;
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
    public int compareTo(InvalidPropertyError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      UnexpectedKey o = (UnexpectedKey) other;
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
