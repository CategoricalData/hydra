// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class AccessorDeclarations implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.AccessorDeclarations");
  
  public static final hydra.core.Name FIELD_NAME_GET = new hydra.core.Name("get");
  
  public static final hydra.core.Name FIELD_NAME_SET = new hydra.core.Name("set");
  
  private AccessorDeclarations () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Get instance) ;
    
    R visit(Set instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AccessorDeclarations instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Get instance) {
      return otherwise((instance));
    }
    
    default R visit(Set instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Get extends hydra.ext.csharp.syntax.AccessorDeclarations implements Serializable {
    public final hydra.util.Opt<hydra.ext.csharp.syntax.AccessorDeclaration> value;
    
    public Get (hydra.util.Opt<hydra.ext.csharp.syntax.AccessorDeclaration> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Get)) {
        return false;
      }
      Get o = (Get) (other);
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
  
  public static final class Set extends hydra.ext.csharp.syntax.AccessorDeclarations implements Serializable {
    public final hydra.util.Opt<hydra.ext.csharp.syntax.AccessorDeclaration> value;
    
    public Set (hydra.util.Opt<hydra.ext.csharp.syntax.AccessorDeclaration> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
        return false;
      }
      Set o = (Set) (other);
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