// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphql.syntax;

import java.io.Serializable;

public abstract class BooleanValue implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphql.syntax.BooleanValue");
  
  public static final hydra.core.Name FIELD_NAME_TRUE = new hydra.core.Name("true");
  
  public static final hydra.core.Name FIELD_NAME_FALSE = new hydra.core.Name("false");
  
  private BooleanValue () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(True instance) ;
    
    R visit(False instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BooleanValue instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(True instance) {
      return otherwise((instance));
    }
    
    default R visit(False instance) {
      return otherwise((instance));
    }
  }
  
  public static final class True extends hydra.ext.org.graphql.syntax.BooleanValue implements Serializable {
    public True () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof True)) {
        return false;
      }
      True o = (True) (other);
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
  
  public static final class False extends hydra.ext.org.graphql.syntax.BooleanValue implements Serializable {
    public False () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof False)) {
        return false;
      }
      False o = (False) (other);
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