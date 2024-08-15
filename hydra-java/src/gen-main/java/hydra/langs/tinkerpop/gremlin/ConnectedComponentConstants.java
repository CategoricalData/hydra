// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class ConnectedComponentConstants implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.ConnectedComponentConstants");
  
  public static final hydra.core.Name FIELD_NAME_COMPONENT = new hydra.core.Name("component");
  
  public static final hydra.core.Name FIELD_NAME_EDGES = new hydra.core.Name("edges");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY_NAME = new hydra.core.Name("propertyName");
  
  private ConnectedComponentConstants () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Component instance) ;
    
    R visit(Edges instance) ;
    
    R visit(PropertyName instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ConnectedComponentConstants instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Component instance) {
      return otherwise((instance));
    }
    
    default R visit(Edges instance) {
      return otherwise((instance));
    }
    
    default R visit(PropertyName instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Component extends hydra.langs.tinkerpop.gremlin.ConnectedComponentConstants implements Serializable {
    public Component () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Component)) {
        return false;
      }
      Component o = (Component) (other);
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
  
  public static final class Edges extends hydra.langs.tinkerpop.gremlin.ConnectedComponentConstants implements Serializable {
    public Edges () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Edges)) {
        return false;
      }
      Edges o = (Edges) (other);
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
  
  public static final class PropertyName extends hydra.langs.tinkerpop.gremlin.ConnectedComponentConstants implements Serializable {
    public PropertyName () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PropertyName)) {
        return false;
      }
      PropertyName o = (PropertyName) (other);
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