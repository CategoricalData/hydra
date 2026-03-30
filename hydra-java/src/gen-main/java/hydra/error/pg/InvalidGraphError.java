// Note: this is an automatically generated file. Do not edit.

package hydra.error.pg;

import java.io.Serializable;

/**
 * An error indicating that a property graph is invalid
 */
public abstract class InvalidGraphError<V> implements Serializable, Comparable<InvalidGraphError<V>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.error.pg.InvalidGraphError");

  public static final hydra.core.Name EDGE = new hydra.core.Name("edge");

  public static final hydra.core.Name VERTEX = new hydra.core.Name("vertex");

  private InvalidGraphError () {

  }

  public abstract <R> R accept(Visitor<V, R> visitor) ;

  public interface Visitor<V, R> {
    R visit(Edge<V> instance) ;

    R visit(Vertex<V> instance) ;
  }

  public interface PartialVisitor<V, R> extends Visitor<V, R> {
    default R otherwise(InvalidGraphError<V> instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Edge<V> instance) {
      return otherwise(instance);
    }

    default R visit(Vertex<V> instance) {
      return otherwise(instance);
    }
  }

  /**
   * An edge in the graph is invalid
   */
  public static final class Edge<V> extends hydra.error.pg.InvalidGraphError<V> implements Serializable {
    public final hydra.error.pg.InvalidGraphEdgeError<V> value;

    public Edge (hydra.error.pg.InvalidGraphEdgeError<V> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Edge)) {
        return false;
      }
      Edge o = (Edge) other;
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
    public int compareTo(InvalidGraphError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Edge o = (Edge) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<V, R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A vertex in the graph is invalid
   */
  public static final class Vertex<V> extends hydra.error.pg.InvalidGraphError<V> implements Serializable {
    public final hydra.error.pg.InvalidGraphVertexError<V> value;

    public Vertex (hydra.error.pg.InvalidGraphVertexError<V> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Vertex)) {
        return false;
      }
      Vertex o = (Vertex) other;
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
    public int compareTo(InvalidGraphError other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Vertex o = (Vertex) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<V, R> visitor) {
      return visitor.visit(this);
    }
  }
}
