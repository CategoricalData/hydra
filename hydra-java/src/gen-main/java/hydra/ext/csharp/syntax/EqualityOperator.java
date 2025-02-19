// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class EqualityOperator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.EqualityOperator");
  
  public static final hydra.core.Name FIELD_NAME_EQUAL = new hydra.core.Name("equal");
  
  public static final hydra.core.Name FIELD_NAME_NOT_EQUAL = new hydra.core.Name("notEqual");
  
  private EqualityOperator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Equal instance) ;
    
    R visit(NotEqual instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EqualityOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Equal instance) {
      return otherwise((instance));
    }
    
    default R visit(NotEqual instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Equal extends hydra.ext.csharp.syntax.EqualityOperator implements Serializable {
    public Equal () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Equal)) {
        return false;
      }
      Equal o = (Equal) (other);
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
  
  public static final class NotEqual extends hydra.ext.csharp.syntax.EqualityOperator implements Serializable {
    public NotEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotEqual)) {
        return false;
      }
      NotEqual o = (NotEqual) (other);
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