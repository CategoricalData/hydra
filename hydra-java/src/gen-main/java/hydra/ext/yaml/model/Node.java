package hydra.ext.yaml.model;

/**
 * A YAML node (value)
 */
public abstract class Node {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/yaml/model.Node");
  
  private Node () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Mapping instance) ;
    
    R visit(Scalar instance) ;
    
    R visit(Sequence instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Node instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Mapping instance) {
      return otherwise((instance));
    }
    
    default R visit(Scalar instance) {
      return otherwise((instance));
    }
    
    default R visit(Sequence instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Mapping extends hydra.ext.yaml.model.Node {
    public final java.util.Map<hydra.ext.yaml.model.Node, hydra.ext.yaml.model.Node> value;
    
    public Mapping (java.util.Map<hydra.ext.yaml.model.Node, hydra.ext.yaml.model.Node> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Mapping)) {
        return false;
      }
      Mapping o = (Mapping) (other);
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
  
  public static final class Scalar extends hydra.ext.yaml.model.Node {
    public final hydra.ext.yaml.model.Scalar value;
    
    public Scalar (hydra.ext.yaml.model.Scalar value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Scalar)) {
        return false;
      }
      Scalar o = (Scalar) (other);
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
  
  public static final class Sequence extends hydra.ext.yaml.model.Node {
    public final java.util.List<hydra.ext.yaml.model.Node> value;
    
    public Sequence (java.util.List<hydra.ext.yaml.model.Node> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence)) {
        return false;
      }
      Sequence o = (Sequence) (other);
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