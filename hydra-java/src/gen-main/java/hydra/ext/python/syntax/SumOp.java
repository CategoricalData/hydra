// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class SumOp implements Serializable, Comparable<SumOp> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.SumOp");
  
  public static final hydra.core.Name FIELD_NAME_ADD = new hydra.core.Name("add");
  
  public static final hydra.core.Name FIELD_NAME_SUB = new hydra.core.Name("sub");
  
  private SumOp () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Add instance) ;
    
    R visit(Sub instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SumOp instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Add instance) {
      return otherwise(instance);
    }
    
    default R visit(Sub instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Add extends hydra.ext.python.syntax.SumOp implements Serializable {
    public Add () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Add)) {
        return false;
      }
      Add o = (Add) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SumOp other) {
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
  
  public static final class Sub extends hydra.ext.python.syntax.SumOp implements Serializable {
    public Sub () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sub)) {
        return false;
      }
      Sub o = (Sub) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SumOp other) {
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
