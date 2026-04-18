// Note: this is an automatically generated file. Do not edit.

package hydra.graphviz.dot;

import java.io.Serializable;

public abstract class NodeOrSubgraph implements Serializable, Comparable<NodeOrSubgraph> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.graphviz.dot.NodeOrSubgraph");

  public static final hydra.core.Name NODE = new hydra.core.Name("node");

  public static final hydra.core.Name SUBGRAPH = new hydra.core.Name("subgraph");

  private NodeOrSubgraph () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Node instance) ;

    R visit(Subgraph instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NodeOrSubgraph instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Node instance) {
      return otherwise(instance);
    }

    default R visit(Subgraph instance) {
      return otherwise(instance);
    }
  }

  public static final class Node extends hydra.graphviz.dot.NodeOrSubgraph implements Serializable {
    public final hydra.graphviz.dot.NodeId value;

    public Node (hydra.graphviz.dot.NodeId value) {
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
    public int compareTo(NodeOrSubgraph other) {
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

  public static final class Subgraph extends hydra.graphviz.dot.NodeOrSubgraph implements Serializable {
    public final hydra.graphviz.dot.Subgraph value;

    public Subgraph (hydra.graphviz.dot.Subgraph value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Subgraph)) {
        return false;
      }
      Subgraph o = (Subgraph) other;
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
    public int compareTo(NodeOrSubgraph other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Subgraph o = (Subgraph) other;
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
