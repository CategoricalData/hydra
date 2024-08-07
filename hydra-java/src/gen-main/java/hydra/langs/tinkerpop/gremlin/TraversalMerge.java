// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalMerge implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.TraversalMerge");
  
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
  
  public static final class OnCreate extends hydra.langs.tinkerpop.gremlin.TraversalMerge implements Serializable {
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
  
  public static final class OnMatch extends hydra.langs.tinkerpop.gremlin.TraversalMerge implements Serializable {
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
  
  public static final class OutV extends hydra.langs.tinkerpop.gremlin.TraversalMerge implements Serializable {
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
  
  public static final class InV extends hydra.langs.tinkerpop.gremlin.TraversalMerge implements Serializable {
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