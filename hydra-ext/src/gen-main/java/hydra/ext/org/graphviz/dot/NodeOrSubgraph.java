// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphviz.dot;

import java.io.Serializable;

public abstract class NodeOrSubgraph implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/graphviz/dot.NodeOrSubgraph");
  
  public static final hydra.core.Name FIELD_NAME_NODE = new hydra.core.Name("node");
  
  public static final hydra.core.Name FIELD_NAME_SUBGRAPH = new hydra.core.Name("subgraph");
  
  private NodeOrSubgraph () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Node instance) ;
    
    R visit(Subgraph instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NodeOrSubgraph instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Node instance) {
      return otherwise((instance));
    }
    
    default R visit(Subgraph instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Node extends hydra.ext.org.graphviz.dot.NodeOrSubgraph implements Serializable {
    public final hydra.ext.org.graphviz.dot.NodeId value;
    
    public Node (hydra.ext.org.graphviz.dot.NodeId value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Node)) {
        return false;
      }
      Node o = (Node) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Subgraph extends hydra.ext.org.graphviz.dot.NodeOrSubgraph implements Serializable {
    public final hydra.ext.org.graphviz.dot.Subgraph value;
    
    public Subgraph (hydra.ext.org.graphviz.dot.Subgraph value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Subgraph)) {
        return false;
      }
      Subgraph o = (Subgraph) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}