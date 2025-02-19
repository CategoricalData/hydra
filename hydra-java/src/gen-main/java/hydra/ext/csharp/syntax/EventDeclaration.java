// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class EventDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.EventDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_STANDARD = new hydra.core.Name("standard");
  
  public static final hydra.core.Name FIELD_NAME_ACCESSORS = new hydra.core.Name("accessors");
  
  private EventDeclaration () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Standard instance) ;
    
    R visit(Accessors instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EventDeclaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Standard instance) {
      return otherwise((instance));
    }
    
    default R visit(Accessors instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Standard extends hydra.ext.csharp.syntax.EventDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.StandardEventDeclaration value;
    
    public Standard (hydra.ext.csharp.syntax.StandardEventDeclaration value) {
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
  
  public static final class Accessors extends hydra.ext.csharp.syntax.EventDeclaration implements Serializable {
    public final hydra.ext.csharp.syntax.AccessorsEventDeclaration value;
    
    public Accessors (hydra.ext.csharp.syntax.AccessorsEventDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Accessors)) {
        return false;
      }
      Accessors o = (Accessors) (other);
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