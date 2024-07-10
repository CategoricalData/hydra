// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class ShortestPathConstants implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.ShortestPathConstants");
  
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
  
  public static final class Target extends hydra.langs.tinkerpop.gremlin.ShortestPathConstants implements Serializable {
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
  
  public static final class Edges extends hydra.langs.tinkerpop.gremlin.ShortestPathConstants implements Serializable {
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
  
  public static final class Distance extends hydra.langs.tinkerpop.gremlin.ShortestPathConstants implements Serializable {
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
  
  public static final class MaxDistance extends hydra.langs.tinkerpop.gremlin.ShortestPathConstants implements Serializable {
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
  
  public static final class IncludeEdges extends hydra.langs.tinkerpop.gremlin.ShortestPathConstants implements Serializable {
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