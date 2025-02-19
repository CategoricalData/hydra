// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class IfTail implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.IfTail");
  
  public static final hydra.core.Name FIELD_NAME_ELIF = new hydra.core.Name("elif");
  
  public static final hydra.core.Name FIELD_NAME_ELSE = new hydra.core.Name("else");
  
  private IfTail () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Elif instance) ;
    
    R visit(Else instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(IfTail instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Elif instance) {
      return otherwise((instance));
    }
    
    default R visit(Else instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Elif extends hydra.ext.python.syntax.IfTail implements Serializable {
    public final hydra.ext.python.syntax.IfStatement value;
    
    public Elif (hydra.ext.python.syntax.IfStatement value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Elif)) {
        return false;
      }
      Elif o = (Elif) (other);
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
  
  public static final class Else extends hydra.ext.python.syntax.IfTail implements Serializable {
    public final hydra.ext.python.syntax.Block value;
    
    public Else (hydra.ext.python.syntax.Block value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Else)) {
        return false;
      }
      Else o = (Else) (other);
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