// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class PrimaryExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.PrimaryExpression");
  
  public static final hydra.core.Name FIELD_NAME_NO_ARRAY = new hydra.core.Name("noArray");
  
  public static final hydra.core.Name FIELD_NAME_ARRAY = new hydra.core.Name("array");
  
  private PrimaryExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(NoArray instance) ;
    
    R visit(Array instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PrimaryExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(NoArray instance) {
      return otherwise((instance));
    }
    
    default R visit(Array instance) {
      return otherwise((instance));
    }
  }
  
  public static final class NoArray extends hydra.ext.csharp.syntax.PrimaryExpression implements Serializable {
    public final hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression value;
    
    public NoArray (hydra.ext.csharp.syntax.PrimaryNoArrayCreationExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NoArray)) {
        return false;
      }
      NoArray o = (NoArray) (other);
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
  
  public static final class Array extends hydra.ext.csharp.syntax.PrimaryExpression implements Serializable {
    public final hydra.ext.csharp.syntax.ArrayCreationExpression value;
    
    public Array (hydra.ext.csharp.syntax.ArrayCreationExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Array)) {
        return false;
      }
      Array o = (Array) (other);
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