// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class SecondaryConstraint implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.SecondaryConstraint");
  
  public static final hydra.core.Name FIELD_NAME_INTERFACE = new hydra.core.Name("interface");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETER = new hydra.core.Name("parameter");
  
  private SecondaryConstraint () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Interface instance) ;
    
    R visit(Parameter instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SecondaryConstraint instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Interface instance) {
      return otherwise((instance));
    }
    
    default R visit(Parameter instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Interface extends hydra.ext.csharp.syntax.SecondaryConstraint implements Serializable {
    public final hydra.ext.csharp.syntax.InterfaceType value;
    
    public Interface (hydra.ext.csharp.syntax.InterfaceType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Interface)) {
        return false;
      }
      Interface o = (Interface) (other);
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
  
  public static final class Parameter extends hydra.ext.csharp.syntax.SecondaryConstraint implements Serializable {
    public final hydra.ext.csharp.syntax.TypeParameter value;
    
    public Parameter (hydra.ext.csharp.syntax.TypeParameter value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parameter)) {
        return false;
      }
      Parameter o = (Parameter) (other);
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