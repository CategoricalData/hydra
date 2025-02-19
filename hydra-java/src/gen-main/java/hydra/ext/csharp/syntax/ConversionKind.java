// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class ConversionKind implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ConversionKind");
  
  public static final hydra.core.Name FIELD_NAME_IMPLICIT = new hydra.core.Name("implicit");
  
  public static final hydra.core.Name FIELD_NAME_EXPLICIT = new hydra.core.Name("explicit");
  
  private ConversionKind () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Implicit instance) ;
    
    R visit(Explicit instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ConversionKind instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Implicit instance) {
      return otherwise((instance));
    }
    
    default R visit(Explicit instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Implicit extends hydra.ext.csharp.syntax.ConversionKind implements Serializable {
    public Implicit () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Implicit)) {
        return false;
      }
      Implicit o = (Implicit) (other);
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
  
  public static final class Explicit extends hydra.ext.csharp.syntax.ConversionKind implements Serializable {
    public Explicit () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Explicit)) {
        return false;
      }
      Explicit o = (Explicit) (other);
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