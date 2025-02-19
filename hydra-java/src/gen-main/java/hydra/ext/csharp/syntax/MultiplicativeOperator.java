// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class MultiplicativeOperator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.MultiplicativeOperator");
  
  public static final hydra.core.Name FIELD_NAME_TIMES = new hydra.core.Name("times");
  
  public static final hydra.core.Name FIELD_NAME_DIVIDE = new hydra.core.Name("divide");
  
  public static final hydra.core.Name FIELD_NAME_MODULO = new hydra.core.Name("modulo");
  
  private MultiplicativeOperator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Times instance) ;
    
    R visit(Divide instance) ;
    
    R visit(Modulo instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(MultiplicativeOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Times instance) {
      return otherwise((instance));
    }
    
    default R visit(Divide instance) {
      return otherwise((instance));
    }
    
    default R visit(Modulo instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Times extends hydra.ext.csharp.syntax.MultiplicativeOperator implements Serializable {
    public Times () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Times)) {
        return false;
      }
      Times o = (Times) (other);
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
  
  public static final class Divide extends hydra.ext.csharp.syntax.MultiplicativeOperator implements Serializable {
    public Divide () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Divide)) {
        return false;
      }
      Divide o = (Divide) (other);
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
  
  public static final class Modulo extends hydra.ext.csharp.syntax.MultiplicativeOperator implements Serializable {
    public Modulo () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Modulo)) {
        return false;
      }
      Modulo o = (Modulo) (other);
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