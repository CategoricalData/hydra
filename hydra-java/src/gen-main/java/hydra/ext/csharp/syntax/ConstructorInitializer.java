// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class ConstructorInitializer implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ConstructorInitializer");
  
  public static final hydra.core.Name FIELD_NAME_BASE = new hydra.core.Name("base");
  
  public static final hydra.core.Name FIELD_NAME_THIS = new hydra.core.Name("this");
  
  private ConstructorInitializer () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Base instance) ;
    
    R visit(This instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ConstructorInitializer instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Base instance) {
      return otherwise((instance));
    }
    
    default R visit(This instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Base extends hydra.ext.csharp.syntax.ConstructorInitializer implements Serializable {
    public final hydra.util.Opt<hydra.ext.csharp.syntax.ArgumentList> value;
    
    public Base (hydra.util.Opt<hydra.ext.csharp.syntax.ArgumentList> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Base)) {
        return false;
      }
      Base o = (Base) (other);
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
  
  public static final class This extends hydra.ext.csharp.syntax.ConstructorInitializer implements Serializable {
    public final hydra.util.Opt<hydra.ext.csharp.syntax.ArgumentList> value;
    
    public This (hydra.util.Opt<hydra.ext.csharp.syntax.ArgumentList> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof This)) {
        return false;
      }
      This o = (This) (other);
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