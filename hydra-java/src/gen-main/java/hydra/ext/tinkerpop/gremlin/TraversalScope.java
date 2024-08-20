// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalScope implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/gremlin.TraversalScope");
  
  public static final hydra.core.Name FIELD_NAME_LOCAL = new hydra.core.Name("local");
  
  public static final hydra.core.Name FIELD_NAME_GLOBAL = new hydra.core.Name("global");
  
  private TraversalScope () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Local instance) ;
    
    R visit(Global instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TraversalScope instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Local instance) {
      return otherwise((instance));
    }
    
    default R visit(Global instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Local extends hydra.ext.tinkerpop.gremlin.TraversalScope implements Serializable {
    public Local () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Local)) {
        return false;
      }
      Local o = (Local) (other);
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
  
  public static final class Global extends hydra.ext.tinkerpop.gremlin.TraversalScope implements Serializable {
    public Global () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Global)) {
        return false;
      }
      Global o = (Global) (other);
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
