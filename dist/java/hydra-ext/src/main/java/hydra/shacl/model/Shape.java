// Note: this is an automatically generated file. Do not edit.

package hydra.shacl.model;

import java.io.Serializable;

/**
 * A SHACL node or property shape. See https://www.w3.org/TR/shacl/#shapes
 */
public abstract class Shape implements Serializable, Comparable<Shape> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shacl.model.Shape");

  public static final hydra.core.Name NODE = new hydra.core.Name("node");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  private Shape () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Node instance) ;

    R visit(Property instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Shape instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Node instance) {
      return otherwise(instance);
    }

    default R visit(Property instance) {
      return otherwise(instance);
    }
  }

  public static final class Node extends hydra.shacl.model.Shape implements Serializable {
    public final hydra.shacl.model.NodeShape value;

    public Node (hydra.shacl.model.NodeShape value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Node)) {
        return false;
      }
      Node o = (Node) other;
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
    public int compareTo(Shape other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Node o = (Node) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Property extends hydra.shacl.model.Shape implements Serializable {
    public final hydra.shacl.model.PropertyShape value;

    public Property (hydra.shacl.model.PropertyShape value) {
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
    public int compareTo(Shape other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Property o = (Property) other;
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
