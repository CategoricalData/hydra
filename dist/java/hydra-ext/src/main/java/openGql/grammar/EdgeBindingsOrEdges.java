// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class EdgeBindingsOrEdges implements Serializable, Comparable<EdgeBindingsOrEdges> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EdgeBindingsOrEdges");

  public static final hydra.core.Name EDGE_BINDINGS = new hydra.core.Name("edgeBindings");

  public static final hydra.core.Name EDGES = new hydra.core.Name("edges");

  private EdgeBindingsOrEdges () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(EdgeBindings instance) ;

    R visit(Edges instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EdgeBindingsOrEdges instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(EdgeBindings instance) {
      return otherwise(instance);
    }

    default R visit(Edges instance) {
      return otherwise(instance);
    }
  }

  public static final class EdgeBindings extends openGql.grammar.EdgeBindingsOrEdges implements Serializable {
    public final Boolean value;

    public EdgeBindings (Boolean value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EdgeBindings)) {
        return false;
      }
      EdgeBindings o = (EdgeBindings) other;
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
    public int compareTo(EdgeBindingsOrEdges other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      EdgeBindings o = (EdgeBindings) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Edges extends openGql.grammar.EdgeBindingsOrEdges implements Serializable {
    public Edges () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Edges)) {
        return false;
      }
      Edges o = (Edges) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(EdgeBindingsOrEdges other) {
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
}
