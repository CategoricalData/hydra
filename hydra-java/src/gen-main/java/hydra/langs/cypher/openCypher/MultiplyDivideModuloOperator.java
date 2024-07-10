// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class MultiplyDivideModuloOperator implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.MultiplyDivideModuloOperator");
  
  private MultiplyDivideModuloOperator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Multiply instance) ;
    
    R visit(Divide instance) ;
    
    R visit(Modulo instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(MultiplyDivideModuloOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Multiply instance) {
      return otherwise((instance));
    }
    
    default R visit(Divide instance) {
      return otherwise((instance));
    }
    
    default R visit(Modulo instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Multiply extends hydra.langs.cypher.openCypher.MultiplyDivideModuloOperator implements Serializable {
    public Multiply () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Multiply)) {
        return false;
      }
      Multiply o = (Multiply) (other);
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
  
  public static final class Divide extends hydra.langs.cypher.openCypher.MultiplyDivideModuloOperator implements Serializable {
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
  
  public static final class Modulo extends hydra.langs.cypher.openCypher.MultiplyDivideModuloOperator implements Serializable {
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