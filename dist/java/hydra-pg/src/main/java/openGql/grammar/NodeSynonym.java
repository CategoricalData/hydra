// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class NodeSynonym implements Serializable, Comparable<NodeSynonym> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NodeSynonym");

  public static final hydra.core.Name NODE = new hydra.core.Name("node");

  public static final hydra.core.Name VERTEX = new hydra.core.Name("vertex");

  private NodeSynonym () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Node instance) ;

    R visit(Vertex instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NodeSynonym instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Node instance) {
      return otherwise(instance);
    }

    default R visit(Vertex instance) {
      return otherwise(instance);
    }
  }

  public static final class Node extends openGql.grammar.NodeSynonym implements Serializable {
    public Node () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Node)) {
        return false;
      }
      Node o = (Node) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(NodeSynonym other) {
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

  public static final class Vertex extends openGql.grammar.NodeSynonym implements Serializable {
    public Vertex () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Vertex)) {
        return false;
      }
      Vertex o = (Vertex) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(NodeSynonym other) {
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
