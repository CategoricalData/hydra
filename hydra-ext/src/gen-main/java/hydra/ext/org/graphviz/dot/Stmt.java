// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphviz.dot;

import java.io.Serializable;

public abstract class Stmt implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/graphviz/dot.Stmt");
  
  public static final hydra.core.Name FIELD_NAME_NODE = new hydra.core.Name("node");
  
  public static final hydra.core.Name FIELD_NAME_EDGE = new hydra.core.Name("edge");
  
  public static final hydra.core.Name FIELD_NAME_ATTR = new hydra.core.Name("attr");
  
  public static final hydra.core.Name FIELD_NAME_EQUALS = new hydra.core.Name("equals");
  
  public static final hydra.core.Name FIELD_NAME_SUBGRAPH = new hydra.core.Name("subgraph");
  
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Node instance) {
      return otherwise((instance));
    }
    
    default R visit(Edge instance) {
      return otherwise((instance));
    }
    
    default R visit(Attr instance) {
      return otherwise((instance));
    }
    
    default R visit(Equals instance) {
      return otherwise((instance));
    }
    
    default R visit(Subgraph instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Node extends hydra.ext.org.graphviz.dot.Stmt implements Serializable {
    public final hydra.ext.org.graphviz.dot.NodeStmt value;
    
    public Node (hydra.ext.org.graphviz.dot.NodeStmt value) {
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
  
  public static final class Edge extends hydra.ext.org.graphviz.dot.Stmt implements Serializable {
    public final hydra.ext.org.graphviz.dot.EdgeStmt value;
    
    public Edge (hydra.ext.org.graphviz.dot.EdgeStmt value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Edge)) {
        return false;
      }
      Edge o = (Edge) (other);
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
  
  public static final class Attr extends hydra.ext.org.graphviz.dot.Stmt implements Serializable {
    public final hydra.ext.org.graphviz.dot.AttrStmt value;
    
    public Attr (hydra.ext.org.graphviz.dot.AttrStmt value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Attr)) {
        return false;
      }
      Attr o = (Attr) (other);
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
  
  public static final class Equals extends hydra.ext.org.graphviz.dot.Stmt implements Serializable {
    public final hydra.ext.org.graphviz.dot.EqualityPair value;
    
    public Equals (hydra.ext.org.graphviz.dot.EqualityPair value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Equals)) {
        return false;
      }
      Equals o = (Equals) (other);
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
  
  public static final class Subgraph extends hydra.ext.org.graphviz.dot.Stmt implements Serializable {
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