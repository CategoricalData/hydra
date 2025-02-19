// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class EventAccessorDeclarations implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.EventAccessorDeclarations");
  
  public static final hydra.core.Name FIELD_NAME_ADD = new hydra.core.Name("add");
  
  public static final hydra.core.Name FIELD_NAME_REMOVE = new hydra.core.Name("remove");
  
  private EventAccessorDeclarations () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Add instance) ;
    
    R visit(Remove instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EventAccessorDeclarations instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Add instance) {
      return otherwise((instance));
    }
    
    default R visit(Remove instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Add extends hydra.ext.csharp.syntax.EventAccessorDeclarations implements Serializable {
    public final hydra.ext.csharp.syntax.AddRemoveAccessorDeclaration value;
    
    public Add (hydra.ext.csharp.syntax.AddRemoveAccessorDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Add)) {
        return false;
      }
      Add o = (Add) (other);
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
  
  public static final class Remove extends hydra.ext.csharp.syntax.EventAccessorDeclarations implements Serializable {
    public final hydra.ext.csharp.syntax.AddRemoveAccessorDeclaration value;
    
    public Remove (hydra.ext.csharp.syntax.AddRemoveAccessorDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Remove)) {
        return false;
      }
      Remove o = (Remove) (other);
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