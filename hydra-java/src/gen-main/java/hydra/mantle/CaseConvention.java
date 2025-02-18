// Note: this is an automatically generated file. Do not edit.

package hydra.mantle;

import java.io.Serializable;

public abstract class CaseConvention implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/mantle.CaseConvention");
  
  public static final hydra.core.Name FIELD_NAME_CAMEL = new hydra.core.Name("camel");
  
  public static final hydra.core.Name FIELD_NAME_PASCAL = new hydra.core.Name("pascal");
  
  public static final hydra.core.Name FIELD_NAME_LOWER_SNAKE = new hydra.core.Name("lowerSnake");
  
  public static final hydra.core.Name FIELD_NAME_UPPER_SNAKE = new hydra.core.Name("upperSnake");
  
  private CaseConvention () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Camel instance) ;
    
    R visit(Pascal instance) ;
    
    R visit(LowerSnake instance) ;
    
    R visit(UpperSnake instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CaseConvention instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Camel instance) {
      return otherwise((instance));
    }
    
    default R visit(Pascal instance) {
      return otherwise((instance));
    }
    
    default R visit(LowerSnake instance) {
      return otherwise((instance));
    }
    
    default R visit(UpperSnake instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Camel extends hydra.mantle.CaseConvention implements Serializable {
    public Camel () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Camel)) {
        return false;
      }
      Camel o = (Camel) (other);
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
  
  public static final class Pascal extends hydra.mantle.CaseConvention implements Serializable {
    public Pascal () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pascal)) {
        return false;
      }
      Pascal o = (Pascal) (other);
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
  
  public static final class LowerSnake extends hydra.mantle.CaseConvention implements Serializable {
    public LowerSnake () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LowerSnake)) {
        return false;
      }
      LowerSnake o = (LowerSnake) (other);
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
  
  public static final class UpperSnake extends hydra.mantle.CaseConvention implements Serializable {
    public UpperSnake () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UpperSnake)) {
        return false;
      }
      UpperSnake o = (UpperSnake) (other);
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