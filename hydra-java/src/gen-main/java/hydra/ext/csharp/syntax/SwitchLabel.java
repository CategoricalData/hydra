// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class SwitchLabel implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.SwitchLabel");
  
  public static final hydra.core.Name FIELD_NAME_BRANCH = new hydra.core.Name("branch");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT = new hydra.core.Name("default");
  
  private SwitchLabel () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Branch instance) ;
    
    R visit(Default instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SwitchLabel instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Branch instance) {
      return otherwise((instance));
    }
    
    default R visit(Default instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Branch extends hydra.ext.csharp.syntax.SwitchLabel implements Serializable {
    public final hydra.ext.csharp.syntax.SwitchBranch value;
    
    public Branch (hydra.ext.csharp.syntax.SwitchBranch value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Branch)) {
        return false;
      }
      Branch o = (Branch) (other);
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
  
  public static final class Default extends hydra.ext.csharp.syntax.SwitchLabel implements Serializable {
    public Default () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Default)) {
        return false;
      }
      Default o = (Default) (other);
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