// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class SelectionStatement implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.SelectionStatement");
  
  public static final hydra.core.Name FIELD_NAME_IF = new hydra.core.Name("if");
  
  public static final hydra.core.Name FIELD_NAME_SWITCH = new hydra.core.Name("switch");
  
  private SelectionStatement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(If instance) ;
    
    R visit(Switch instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SelectionStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(If instance) {
      return otherwise((instance));
    }
    
    default R visit(Switch instance) {
      return otherwise((instance));
    }
  }
  
  public static final class If extends hydra.ext.csharp.syntax.SelectionStatement implements Serializable {
    public final hydra.ext.csharp.syntax.IfStatement value;
    
    public If (hydra.ext.csharp.syntax.IfStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof If)) {
        return false;
      }
      If o = (If) (other);
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
  
  public static final class Switch extends hydra.ext.csharp.syntax.SelectionStatement implements Serializable {
    public final hydra.ext.csharp.syntax.SwitchStatement value;
    
    public Switch (hydra.ext.csharp.syntax.SwitchStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Switch)) {
        return false;
      }
      Switch o = (Switch) (other);
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