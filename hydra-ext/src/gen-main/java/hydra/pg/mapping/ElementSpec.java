// Note: this is an automatically generated file. Do not edit.

package hydra.pg.mapping;

import java.io.Serializable;

/**
 * Either a vertex specification or an edge specification
 */
public abstract class ElementSpec implements Serializable, Comparable<ElementSpec> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.mapping.ElementSpec");

  public static final hydra.core.Name VERTEX = new hydra.core.Name("vertex");

  public static final hydra.core.Name EDGE = new hydra.core.Name("edge");

  private ElementSpec () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Vertex instance) ;

    R visit(Edge instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ElementSpec instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Vertex instance) {
      return otherwise(instance);
    }

    default R visit(Edge instance) {
      return otherwise(instance);
    }
  }

  public static final class Vertex extends hydra.pg.mapping.ElementSpec implements Serializable {
    public final hydra.pg.mapping.VertexSpec value;

    public Vertex (hydra.pg.mapping.VertexSpec value) {
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
    public int compareTo(ElementSpec other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Vertex o = (Vertex) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Edge extends hydra.pg.mapping.ElementSpec implements Serializable {
    public final hydra.pg.mapping.EdgeSpec value;

    public Edge (hydra.pg.mapping.EdgeSpec value) {
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
    public int compareTo(ElementSpec other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Edge o = (Edge) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
