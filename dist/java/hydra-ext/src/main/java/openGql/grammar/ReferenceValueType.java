// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ReferenceValueType implements Serializable, Comparable<ReferenceValueType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ReferenceValueType");

  public static final hydra.core.Name GRAPH = new hydra.core.Name("graph");

  public static final hydra.core.Name BINDING_TABLE = new hydra.core.Name("bindingTable");

  public static final hydra.core.Name NODE = new hydra.core.Name("node");

  public static final hydra.core.Name EDGE = new hydra.core.Name("edge");

  private ReferenceValueType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Graph instance) ;

    R visit(BindingTable instance) ;

    R visit(Node instance) ;

    R visit(Edge instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ReferenceValueType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Graph instance) {
      return otherwise(instance);
    }

    default R visit(BindingTable instance) {
      return otherwise(instance);
    }

    default R visit(Node instance) {
      return otherwise(instance);
    }

    default R visit(Edge instance) {
      return otherwise(instance);
    }
  }

  public static final class Graph extends openGql.grammar.ReferenceValueType implements Serializable {
    public final openGql.grammar.GraphReferenceValueType value;

    public Graph (openGql.grammar.GraphReferenceValueType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Graph)) {
        return false;
      }
      Graph o = (Graph) other;
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
    public int compareTo(ReferenceValueType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Graph o = (Graph) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class BindingTable extends openGql.grammar.ReferenceValueType implements Serializable {
    public final openGql.grammar.BindingTableReferenceValueType value;

    public BindingTable (openGql.grammar.BindingTableReferenceValueType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BindingTable)) {
        return false;
      }
      BindingTable o = (BindingTable) other;
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
    public int compareTo(ReferenceValueType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      BindingTable o = (BindingTable) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Node extends openGql.grammar.ReferenceValueType implements Serializable {
    public final openGql.grammar.NodeReferenceValueType value;

    public Node (openGql.grammar.NodeReferenceValueType value) {
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
    public int compareTo(ReferenceValueType other) {
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

  public static final class Edge extends openGql.grammar.ReferenceValueType implements Serializable {
    public final openGql.grammar.EdgeReferenceValueType value;

    public Edge (openGql.grammar.EdgeReferenceValueType value) {
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
    public int compareTo(ReferenceValueType other) {
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
}
