// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ElementTypeSpecification implements Serializable, Comparable<ElementTypeSpecification> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ElementTypeSpecification");

  public static final hydra.core.Name NODE_TYPE = new hydra.core.Name("nodeType");

  public static final hydra.core.Name EDGE_TYPE = new hydra.core.Name("edgeType");

  private ElementTypeSpecification () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(NodeType instance) ;

    R visit(EdgeType instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ElementTypeSpecification instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(NodeType instance) {
      return otherwise(instance);
    }

    default R visit(EdgeType instance) {
      return otherwise(instance);
    }
  }

  public static final class NodeType extends openGql.grammar.ElementTypeSpecification implements Serializable {
    public final openGql.grammar.NodeTypeSpecification value;

    public NodeType (openGql.grammar.NodeTypeSpecification value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NodeType)) {
        return false;
      }
      NodeType o = (NodeType) other;
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
    public int compareTo(ElementTypeSpecification other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NodeType o = (NodeType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class EdgeType extends openGql.grammar.ElementTypeSpecification implements Serializable {
    public final openGql.grammar.EdgeTypeSpecification value;

    public EdgeType (openGql.grammar.EdgeTypeSpecification value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EdgeType)) {
        return false;
      }
      EdgeType o = (EdgeType) other;
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
    public int compareTo(ElementTypeSpecification other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      EdgeType o = (EdgeType) other;
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
