// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class Block implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Block");
  
  public static final hydra.core.Name FIELD_NAME_INDENTED = new hydra.core.Name("indented");
  
  public static final hydra.core.Name FIELD_NAME_SIMPLE = new hydra.core.Name("simple");
  
  private Block () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Indented instance) ;
    
    R visit(Simple instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Block instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Indented instance) {
      return otherwise((instance));
    }
    
    default R visit(Simple instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Indented extends hydra.ext.python.syntax.Block implements Serializable {
    public final java.util.List<java.util.List<hydra.ext.python.syntax.Statement>> value;
    
    public Indented (java.util.List<java.util.List<hydra.ext.python.syntax.Statement>> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Indented)) {
        return false;
      }
      Indented o = (Indented) (other);
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
  
  public static final class Simple extends hydra.ext.python.syntax.Block implements Serializable {
    public final java.util.List<hydra.ext.python.syntax.SimpleStatement> value;
    
    public Simple (java.util.List<hydra.ext.python.syntax.SimpleStatement> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) (other);
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