// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class BaseAccess implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.BaseAccess");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  private BaseAccess () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Identifier instance) ;
    
    R visit(Arguments instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BaseAccess instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Identifier instance) {
      return otherwise((instance));
    }
    
    default R visit(Arguments instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Identifier extends hydra.ext.csharp.syntax.BaseAccess implements Serializable {
    public final hydra.ext.csharp.syntax.BaseAccessWithIdentifier value;
    
    public Identifier (hydra.ext.csharp.syntax.BaseAccessWithIdentifier value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Identifier)) {
        return false;
      }
      Identifier o = (Identifier) (other);
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
  
  public static final class Arguments extends hydra.ext.csharp.syntax.BaseAccess implements Serializable {
    public final hydra.ext.csharp.syntax.ArgumentList value;
    
    public Arguments (hydra.ext.csharp.syntax.ArgumentList value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Arguments)) {
        return false;
      }
      Arguments o = (Arguments) (other);
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