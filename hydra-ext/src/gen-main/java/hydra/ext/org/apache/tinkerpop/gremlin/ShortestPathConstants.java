// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class ShortestPathConstants implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ShortestPathConstants");
  
  public static final hydra.core.Name FIELD_NAME_TARGET = new hydra.core.Name("target");
  
  public static final hydra.core.Name FIELD_NAME_EDGES = new hydra.core.Name("edges");
  
  public static final hydra.core.Name FIELD_NAME_DISTANCE = new hydra.core.Name("distance");
  
  public static final hydra.core.Name FIELD_NAME_MAX_DISTANCE = new hydra.core.Name("maxDistance");
  
  public static final hydra.core.Name FIELD_NAME_INCLUDE_EDGES = new hydra.core.Name("includeEdges");
  
  private ShortestPathConstants () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Target instance) ;
    
    R visit(Edges instance) ;
    
    R visit(Distance instance) ;
    
    R visit(MaxDistance instance) ;
    
    R visit(IncludeEdges instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ShortestPathConstants instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Target instance) {
      return otherwise((instance));
    }
    
    default R visit(Edges instance) {
      return otherwise((instance));
    }
    
    default R visit(Distance instance) {
      return otherwise((instance));
    }
    
    default R visit(MaxDistance instance) {
      return otherwise((instance));
    }
    
    default R visit(IncludeEdges instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Target extends hydra.ext.org.apache.tinkerpop.gremlin.ShortestPathConstants implements Serializable {
    public Target () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Target)) {
        return false;
      }
      Target o = (Target) (other);
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
  
  public static final class Edges extends hydra.ext.org.apache.tinkerpop.gremlin.ShortestPathConstants implements Serializable {
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
  
  public static final class Distance extends hydra.ext.org.apache.tinkerpop.gremlin.ShortestPathConstants implements Serializable {
    public Distance () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Distance)) {
        return false;
      }
      Distance o = (Distance) (other);
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
  
  public static final class MaxDistance extends hydra.ext.org.apache.tinkerpop.gremlin.ShortestPathConstants implements Serializable {
    public MaxDistance () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MaxDistance)) {
        return false;
      }
      MaxDistance o = (MaxDistance) (other);
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
  
  public static final class IncludeEdges extends hydra.ext.org.apache.tinkerpop.gremlin.ShortestPathConstants implements Serializable {
    public IncludeEdges () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IncludeEdges)) {
        return false;
      }
      IncludeEdges o = (IncludeEdges) (other);
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