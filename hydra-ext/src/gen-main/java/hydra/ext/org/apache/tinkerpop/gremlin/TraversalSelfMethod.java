// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalSelfMethod implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalSelfMethod");
  
  public static final hydra.core.Name FIELD_NAME_DISCARD = new hydra.core.Name("discard");
  
  private TraversalSelfMethod () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Discard instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TraversalSelfMethod instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Discard instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Discard extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalSelfMethod implements Serializable {
    public Discard () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Discard)) {
        return false;
      }
      Discard o = (Discard) (other);
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