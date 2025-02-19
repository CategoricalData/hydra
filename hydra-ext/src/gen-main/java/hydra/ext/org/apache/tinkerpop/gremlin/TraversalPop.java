// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalPop implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalPop");
  
  public static final hydra.core.Name FIELD_NAME_FIRST = new hydra.core.Name("first");
  
  public static final hydra.core.Name FIELD_NAME_LAST = new hydra.core.Name("last");
  
  public static final hydra.core.Name FIELD_NAME_ALL = new hydra.core.Name("all");
  
  public static final hydra.core.Name FIELD_NAME_MIXED = new hydra.core.Name("mixed");
  
  private TraversalPop () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(First instance) ;
    
    R visit(Last instance) ;
    
    R visit(All instance) ;
    
    R visit(Mixed instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TraversalPop instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(First instance) {
      return otherwise((instance));
    }
    
    default R visit(Last instance) {
      return otherwise((instance));
    }
    
    default R visit(All instance) {
      return otherwise((instance));
    }
    
    default R visit(Mixed instance) {
      return otherwise((instance));
    }
  }
  
  public static final class First extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPop implements Serializable {
    public First () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof First)) {
        return false;
      }
      First o = (First) (other);
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
  
  public static final class Last extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPop implements Serializable {
    public Last () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Last)) {
        return false;
      }
      Last o = (Last) (other);
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
  
  public static final class All extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPop implements Serializable {
    public All () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof All)) {
        return false;
      }
      All o = (All) (other);
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
  
  public static final class Mixed extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPop implements Serializable {
    public Mixed () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Mixed)) {
        return false;
      }
      Mixed o = (Mixed) (other);
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