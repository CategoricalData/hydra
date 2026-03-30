// Note: this is an automatically generated file. Do not edit.

package hydra.error.pg;

import java.io.Serializable;

/**
 * An error indicating that a vertex is invalid
 */
public abstract class InvalidVertexError implements Serializable, Comparable<InvalidVertexError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.pg.InvalidVertexError");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  private InvalidVertexError () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Id instance) ;

    R visit(Label instance) ;

    R visit(Property instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InvalidVertexError instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Id instance) {
      return otherwise(instance);
    }

    default R visit(Label instance) {
      return otherwise(instance);
    }

    default R visit(Property instance) {
      return otherwise(instance);
    }
  }

  /**
   * The vertex id value is invalid
   */
  public static final class Id extends hydra.error.pg.InvalidVertexError implements Serializable {
    public final hydra.error.pg.InvalidValueError value;

    public Id (hydra.error.pg.InvalidValueError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Id)) {
        return false;
      }
      Id o = (Id) other;
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
    public int compareTo(InvalidVertexError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Id o = (Id) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * The vertex label does not exist in the schema
   */
  public static final class Label extends hydra.error.pg.InvalidVertexError implements Serializable {
    public final hydra.error.pg.NoSuchVertexLabelError value;

    public Label (hydra.error.pg.NoSuchVertexLabelError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Label)) {
        return false;
      }
      Label o = (Label) other;
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
    public int compareTo(InvalidVertexError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Label o = (Label) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A property of the vertex is invalid
   */
  public static final class Property extends hydra.error.pg.InvalidVertexError implements Serializable {
    public final hydra.error.pg.InvalidElementPropertyError value;

    public Property (hydra.error.pg.InvalidElementPropertyError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Property)) {
        return false;
      }
      Property o = (Property) other;
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
    public int compareTo(InvalidVertexError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Property o = (Property) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
