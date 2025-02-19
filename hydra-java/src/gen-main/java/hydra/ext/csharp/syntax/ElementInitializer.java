// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class ElementInitializer implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ElementInitializer");
  
  public static final hydra.core.Name FIELD_NAME_SINGLE = new hydra.core.Name("single");
  
  public static final hydra.core.Name FIELD_NAME_LIST = new hydra.core.Name("list");
  
  private ElementInitializer () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Single instance) ;
    
    R visit(List instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ElementInitializer instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Single instance) {
      return otherwise((instance));
    }
    
    default R visit(List instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Single extends hydra.ext.csharp.syntax.ElementInitializer implements Serializable {
    public final hydra.ext.csharp.syntax.NonAssignmentExpression value;
    
    public Single (hydra.ext.csharp.syntax.NonAssignmentExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Single)) {
        return false;
      }
      Single o = (Single) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class List extends hydra.ext.csharp.syntax.ElementInitializer implements Serializable {
    public final hydra.ext.csharp.syntax.ExpressionList value;
    
    public List (hydra.ext.csharp.syntax.ExpressionList value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}