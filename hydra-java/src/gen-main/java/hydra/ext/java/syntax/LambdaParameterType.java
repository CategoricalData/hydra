// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class LambdaParameterType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.LambdaParameterType");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_VAR = new hydra.core.Name("var");
  
  private LambdaParameterType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Type instance) ;
    
    R visit(Var instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LambdaParameterType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Type instance) {
      return otherwise((instance));
    }
    
    default R visit(Var instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Type extends hydra.ext.java.syntax.LambdaParameterType implements Serializable {
    public final hydra.ext.java.syntax.UnannType value;
    
    public Type (hydra.ext.java.syntax.UnannType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Type)) {
        return false;
      }
      Type o = (Type) (other);
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
  
  public static final class Var extends hydra.ext.java.syntax.LambdaParameterType implements Serializable {
    public Var () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Var)) {
        return false;
      }
      Var o = (Var) (other);
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