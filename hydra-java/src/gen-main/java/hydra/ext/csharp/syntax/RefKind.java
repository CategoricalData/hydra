// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class RefKind implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.RefKind");
  
  public static final hydra.core.Name FIELD_NAME_REF = new hydra.core.Name("ref");
  
  public static final hydra.core.Name FIELD_NAME_REF_READONLY = new hydra.core.Name("refReadonly");
  
  private RefKind () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Ref instance) ;
    
    R visit(RefReadonly instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(RefKind instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Ref instance) {
      return otherwise((instance));
    }
    
    default R visit(RefReadonly instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Ref extends hydra.ext.csharp.syntax.RefKind implements Serializable {
    public Ref () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ref)) {
        return false;
      }
      Ref o = (Ref) (other);
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
  
  public static final class RefReadonly extends hydra.ext.csharp.syntax.RefKind implements Serializable {
    public RefReadonly () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RefReadonly)) {
        return false;
      }
      RefReadonly o = (RefReadonly) (other);
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