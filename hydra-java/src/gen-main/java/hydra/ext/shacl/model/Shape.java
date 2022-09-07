package hydra.ext.shacl.model;

/**
 * A SHACL node or property shape. See https://www.w3.org/TR/shacl/#shapes
 */
public abstract class Shape {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shacl/model.Shape");
  
  private Shape () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Node instance) ;
    
    R visit(Property instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Shape instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Node instance) {
      return otherwise((instance));
    }
    
    default R visit(Property instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Node extends hydra.ext.shacl.model.Shape {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shacl/model.Node");
    
    public final hydra.ext.shacl.model.NodeShape value;
    
    public Node (hydra.ext.shacl.model.NodeShape value) {
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
  
  public static final class Property extends hydra.ext.shacl.model.Shape {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/shacl/model.Property");
    
    public final hydra.ext.shacl.model.PropertyShape value;
    
    public Property (hydra.ext.shacl.model.PropertyShape value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Property)) {
        return false;
      }
      Property o = (Property) (other);
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