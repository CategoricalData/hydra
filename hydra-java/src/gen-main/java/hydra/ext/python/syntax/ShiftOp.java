// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class ShiftOp implements Serializable, Comparable<ShiftOp> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ShiftOp");
  
  public static final hydra.core.Name FIELD_NAME_LEFT = new hydra.core.Name("left");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT = new hydra.core.Name("right");
  
  private ShiftOp () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Left instance) ;
    
    R visit(Right instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ShiftOp instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Left instance) {
      return otherwise(instance);
    }
    
    default R visit(Right instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Left extends hydra.ext.python.syntax.ShiftOp implements Serializable {
    public Left () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Left)) {
        return false;
      }
      Left o = (Left) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ShiftOp other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Right extends hydra.ext.python.syntax.ShiftOp implements Serializable {
    public Right () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Right)) {
        return false;
      }
      Right o = (Right) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ShiftOp other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
