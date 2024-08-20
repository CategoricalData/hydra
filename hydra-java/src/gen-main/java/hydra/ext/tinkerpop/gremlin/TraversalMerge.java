// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalMerge implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/gremlin.TraversalMerge");
  
  public static final hydra.core.Name FIELD_NAME_ON_CREATE = new hydra.core.Name("onCreate");
  
  public static final hydra.core.Name FIELD_NAME_ON_MATCH = new hydra.core.Name("onMatch");
  
  public static final hydra.core.Name FIELD_NAME_OUT_V = new hydra.core.Name("outV");
  
  public static final hydra.core.Name FIELD_NAME_IN_V = new hydra.core.Name("inV");
  
  private TraversalMerge () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(OnCreate instance) ;
    
    R visit(OnMatch instance) ;
    
    R visit(OutV instance) ;
    
    R visit(InV instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TraversalMerge instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(OnCreate instance) {
      return otherwise((instance));
    }
    
    default R visit(OnMatch instance) {
      return otherwise((instance));
    }
    
    default R visit(OutV instance) {
      return otherwise((instance));
    }
    
    default R visit(InV instance) {
      return otherwise((instance));
    }
  }
  
  public static final class OnCreate extends hydra.ext.tinkerpop.gremlin.TraversalMerge implements Serializable {
    public OnCreate () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OnCreate)) {
        return false;
      }
      OnCreate o = (OnCreate) (other);
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
  
  public static final class OnMatch extends hydra.ext.tinkerpop.gremlin.TraversalMerge implements Serializable {
    public OnMatch () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OnMatch)) {
        return false;
      }
      OnMatch o = (OnMatch) (other);
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
  
  public static final class OutV extends hydra.ext.tinkerpop.gremlin.TraversalMerge implements Serializable {
    public OutV () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OutV)) {
        return false;
      }
      OutV o = (OutV) (other);
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
  
  public static final class InV extends hydra.ext.tinkerpop.gremlin.TraversalMerge implements Serializable {
    public InV () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InV)) {
        return false;
      }
      InV o = (InV) (other);
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
