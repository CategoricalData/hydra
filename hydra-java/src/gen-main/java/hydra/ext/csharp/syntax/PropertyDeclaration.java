// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class PropertyDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.PropertyDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_STANDARD = new hydra.core.Name("standard");
  
  public static final hydra.core.Name FIELD_NAME_REF_RETURN = new hydra.core.Name("refReturn");
  
  private PropertyDeclaration () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Standard instance) ;
    
    R visit(RefReturn instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PropertyDeclaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Standard instance) {
      return otherwise((instance));
    }
    
    default R visit(RefReturn instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Standard extends hydra.ext.csharp.syntax.PropertyDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.StandardPropertyDeclaration value;
    
    public Standard (hydra.ext.csharp.syntax.StandardPropertyDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Standard)) {
        return false;
      }
      Standard o = (Standard) (other);
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
  
  public static final class RefReturn extends hydra.ext.csharp.syntax.PropertyDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.RefReturnPropertyDeclaration value;
    
    public RefReturn (hydra.ext.csharp.syntax.RefReturnPropertyDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RefReturn)) {
        return false;
      }
      RefReturn o = (RefReturn) (other);
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