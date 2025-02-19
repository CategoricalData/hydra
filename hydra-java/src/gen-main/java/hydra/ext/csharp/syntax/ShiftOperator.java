// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class ShiftOperator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ShiftOperator");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  private ShiftOperator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Left instance) ;
    
    R visit(Right instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ShiftOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Left instance) {
      return otherwise((instance));
    }
    
    default R visit(Right instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Left extends hydra.ext.csharp.syntax.ShiftOperator implements Serializable {
    public Left () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Left)) {
        return false;
      }
      Left o = (Left) (other);
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
  
  public static final class Right extends hydra.ext.csharp.syntax.ShiftOperator implements Serializable {
    public Right () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Right)) {
        return false;
      }
      Right o = (Right) (other);
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