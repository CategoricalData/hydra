// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphviz.dot;

import java.io.Serializable;

public abstract class AttrType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/graphviz/dot.AttrType");
  
  public static final hydra.core.Name FIELD_NAME_GRAPH = new hydra.core.Name("graph");
  
  public static final hydra.core.Name FIELD_NAME_NODE = new hydra.core.Name("node");
  
  public static final hydra.core.Name FIELD_NAME_EDGE = new hydra.core.Name("edge");
  
  private AttrType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Graph instance) ;
    
    R visit(Node instance) ;
    
    R visit(Edge instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AttrType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Graph instance) {
      return otherwise((instance));
    }
    
    default R visit(Node instance) {
      return otherwise((instance));
    }
    
    default R visit(Edge instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Graph extends hydra.ext.org.graphviz.dot.AttrType implements Serializable {
    public Graph () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Graph)) {
        return false;
      }
      Graph o = (Graph) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Node extends hydra.ext.org.graphviz.dot.AttrType implements Serializable {
    public Node () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Node)) {
        return false;
      }
      Node o = (Node) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Edge extends hydra.ext.org.graphviz.dot.AttrType implements Serializable {
    public Edge () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Edge)) {
        return false;
      }
      Edge o = (Edge) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}