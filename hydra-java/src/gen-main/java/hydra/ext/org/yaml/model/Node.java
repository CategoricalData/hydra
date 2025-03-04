// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.yaml.model;

import java.io.Serializable;

/**
 * A YAML node (value)
 */
public abstract class Node implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.yaml.model.Node");
  
  public static final hydra.core.Name FIELD_NAME_MAPPING = new hydra.core.Name("mapping");
  
  public static final hydra.core.Name FIELD_NAME_SCALAR = new hydra.core.Name("scalar");
  
  public static final hydra.core.Name FIELD_NAME_SEQUENCE = new hydra.core.Name("sequence");
  
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
  
  public static final class Mapping extends hydra.ext.org.yaml.model.Node implements Serializable {
    public final java.util.Map<hydra.ext.org.yaml.model.Node, hydra.ext.org.yaml.model.Node> value;
    
    public Mapping (java.util.Map<hydra.ext.org.yaml.model.Node, hydra.ext.org.yaml.model.Node> value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Scalar extends hydra.ext.org.yaml.model.Node implements Serializable {
    public final hydra.ext.org.yaml.model.Scalar value;
    
    public Scalar (hydra.ext.org.yaml.model.Scalar value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Sequence extends hydra.ext.org.yaml.model.Node implements Serializable {
    public final java.util.List<hydra.ext.org.yaml.model.Node> value;
    
    public Sequence (java.util.List<hydra.ext.org.yaml.model.Node> value) {
      java.util.Objects.requireNonNull((value));
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