// Note: this is an automatically generated file. Do not edit.

package hydra.error.pg;

import java.io.Serializable;

/**
 * An error indicating that an edge is invalid
 */
public abstract class InvalidEdgeError implements Serializable, Comparable<InvalidEdgeError> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.pg.InvalidEdgeError");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name IN_VERTEX_LABEL = new hydra.core.Name("inVertexLabel");

  public static final hydra.core.Name IN_VERTEX_NOT_FOUND = new hydra.core.Name("inVertexNotFound");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  public static final hydra.core.Name OUT_VERTEX_LABEL = new hydra.core.Name("outVertexLabel");

  public static final hydra.core.Name OUT_VERTEX_NOT_FOUND = new hydra.core.Name("outVertexNotFound");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  private InvalidEdgeError () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Id instance) ;

    R visit(InVertexLabel instance) ;

    R visit(InVertexNotFound instance) ;

    R visit(Label instance) ;

    R visit(OutVertexLabel instance) ;

    R visit(OutVertexNotFound instance) ;

    R visit(Property instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(InvalidEdgeError instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Id instance) {
      return otherwise(instance);
    }

    default R visit(InVertexLabel instance) {
      return otherwise(instance);
    }

    default R visit(InVertexNotFound instance) {
      return otherwise(instance);
    }

    default R visit(Label instance) {
      return otherwise(instance);
    }

    default R visit(OutVertexLabel instance) {
      return otherwise(instance);
    }

    default R visit(OutVertexNotFound instance) {
      return otherwise(instance);
    }

    default R visit(Property instance) {
      return otherwise(instance);
    }
  }

  /**
   * The edge id value is invalid
   */
  public static final class Id extends hydra.error.pg.InvalidEdgeError implements Serializable {
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
    public int compareTo(InvalidEdgeError other) {
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
   * The in-vertex has the wrong label
   */
  public static final class InVertexLabel extends hydra.error.pg.InvalidEdgeError implements Serializable {
    public final hydra.error.pg.WrongVertexLabelError value;

    public InVertexLabel (hydra.error.pg.WrongVertexLabelError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InVertexLabel)) {
        return false;
      }
      InVertexLabel o = (InVertexLabel) other;
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
    public int compareTo(InvalidEdgeError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      InVertexLabel o = (InVertexLabel) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * The in-vertex does not exist in the graph
   */
  public static final class InVertexNotFound extends hydra.error.pg.InvalidEdgeError implements Serializable {
    public InVertexNotFound () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InVertexNotFound)) {
        return false;
      }
      InVertexNotFound o = (InVertexNotFound) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(InvalidEdgeError other) {
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

  /**
   * The edge label does not exist in the schema
   */
  public static final class Label extends hydra.error.pg.InvalidEdgeError implements Serializable {
    public final hydra.error.pg.NoSuchEdgeLabelError value;

    public Label (hydra.error.pg.NoSuchEdgeLabelError value) {
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
    public int compareTo(InvalidEdgeError other) {
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
   * The out-vertex has the wrong label
   */
  public static final class OutVertexLabel extends hydra.error.pg.InvalidEdgeError implements Serializable {
    public final hydra.error.pg.WrongVertexLabelError value;

    public OutVertexLabel (hydra.error.pg.WrongVertexLabelError value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OutVertexLabel)) {
        return false;
      }
      OutVertexLabel o = (OutVertexLabel) other;
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
    public int compareTo(InvalidEdgeError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      OutVertexLabel o = (OutVertexLabel) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * The out-vertex does not exist in the graph
   */
  public static final class OutVertexNotFound extends hydra.error.pg.InvalidEdgeError implements Serializable {
    public OutVertexNotFound () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OutVertexNotFound)) {
        return false;
      }
      OutVertexNotFound o = (OutVertexNotFound) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(InvalidEdgeError other) {
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

  /**
   * A property of the edge is invalid
   */
  public static final class Property extends hydra.error.pg.InvalidEdgeError implements Serializable {
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
    public int compareTo(InvalidEdgeError other) {
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
