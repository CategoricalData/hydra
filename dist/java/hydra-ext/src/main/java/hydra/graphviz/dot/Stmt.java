// Note: this is an automatically generated file. Do not edit.

package hydra.graphviz.dot;

import java.io.Serializable;

public abstract class Stmt implements Serializable, Comparable<Stmt> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.graphviz.dot.Stmt");

  public static final hydra.core.Name NODE = new hydra.core.Name("node");

  public static final hydra.core.Name EDGE = new hydra.core.Name("edge");

  public static final hydra.core.Name ATTR = new hydra.core.Name("attr");

  public static final hydra.core.Name EQUALS = new hydra.core.Name("equals");

  public static final hydra.core.Name SUBGRAPH = new hydra.core.Name("subgraph");

  private Stmt () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Node instance) ;

    R visit(Edge instance) ;

    R visit(Attr instance) ;

    R visit(Equals instance) ;

    R visit(Subgraph instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Stmt instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Node instance) {
      return otherwise(instance);
    }

    default R visit(Edge instance) {
      return otherwise(instance);
    }

    default R visit(Attr instance) {
      return otherwise(instance);
    }

    default R visit(Equals instance) {
      return otherwise(instance);
    }

    default R visit(Subgraph instance) {
      return otherwise(instance);
    }
  }

  public static final class Node extends hydra.graphviz.dot.Stmt implements Serializable {
    public final hydra.graphviz.dot.NodeStmt value;

    public Node (hydra.graphviz.dot.NodeStmt value) {
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
    public int compareTo(Stmt other) {
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

  public static final class Edge extends hydra.graphviz.dot.Stmt implements Serializable {
    public final hydra.graphviz.dot.EdgeStmt value;

    public Edge (hydra.graphviz.dot.EdgeStmt value) {
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
    public int compareTo(Stmt other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Edge o = (Edge) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Attr extends hydra.graphviz.dot.Stmt implements Serializable {
    public final hydra.graphviz.dot.AttrStmt value;

    public Attr (hydra.graphviz.dot.AttrStmt value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Attr)) {
        return false;
      }
      Attr o = (Attr) other;
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
    public int compareTo(Stmt other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Attr o = (Attr) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Equals extends hydra.graphviz.dot.Stmt implements Serializable {
    public final hydra.graphviz.dot.EqualityPair value;

    public Equals (hydra.graphviz.dot.EqualityPair value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Equals)) {
        return false;
      }
      Equals o = (Equals) other;
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
    public int compareTo(Stmt other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Equals o = (Equals) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Subgraph extends hydra.graphviz.dot.Stmt implements Serializable {
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
    public int compareTo(Stmt other) {
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
