// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class ReturnType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ReturnType");
  
  public static final hydra.core.Name FIELD_NAME_REF = new hydra.core.Name("ref");
  
  public static final hydra.core.Name FIELD_NAME_VOID = new hydra.core.Name("void");
  
  private ReturnType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Ref instance) ;
    
    R visit(Void_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ReturnType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Ref instance) {
      return otherwise((instance));
    }
    
    default R visit(Void_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Ref extends hydra.ext.csharp.syntax.ReturnType implements Serializable {
    public final hydra.ext.csharp.syntax.Type value;
    
    public Ref (hydra.ext.csharp.syntax.Type value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ref)) {
        return false;
      }
      Ref o = (Ref) (other);
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
  
  public static final class Void_ extends hydra.ext.csharp.syntax.ReturnType implements Serializable {
    public Void_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Void_)) {
        return false;
      }
      Void_ o = (Void_) (other);
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