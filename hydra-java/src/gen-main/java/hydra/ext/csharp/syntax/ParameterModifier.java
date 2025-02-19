// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class ParameterModifier implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ParameterModifier");
  
  public static final hydra.core.Name FIELD_NAME_MODE = new hydra.core.Name("mode");
  
  public static final hydra.core.Name FIELD_NAME_THIS = new hydra.core.Name("this");
  
  private ParameterModifier () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Mode instance) ;
    
    R visit(This instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ParameterModifier instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Mode instance) {
      return otherwise((instance));
    }
    
    default R visit(This instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Mode extends hydra.ext.csharp.syntax.ParameterModifier implements Serializable {
    public final hydra.ext.csharp.syntax.ParameterModeModifier value;
    
    public Mode (hydra.ext.csharp.syntax.ParameterModeModifier value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Mode)) {
        return false;
      }
      Mode o = (Mode) (other);
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
  
  public static final class This extends hydra.ext.csharp.syntax.ParameterModifier implements Serializable {
    public This () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof This)) {
        return false;
      }
      This o = (This) (other);
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