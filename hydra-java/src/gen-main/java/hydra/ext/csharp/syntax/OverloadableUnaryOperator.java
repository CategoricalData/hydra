// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class OverloadableUnaryOperator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.OverloadableUnaryOperator");
  
  public static final hydra.core.Name FIELD_NAME_PLUS = new hydra.core.Name("plus");
  
  public static final hydra.core.Name FIELD_NAME_MINUS = new hydra.core.Name("minus");
  
  public static final hydra.core.Name FIELD_NAME_NOT = new hydra.core.Name("not");
  
  public static final hydra.core.Name FIELD_NAME_COMPLEMENT = new hydra.core.Name("complement");
  
  public static final hydra.core.Name FIELD_NAME_INCREMENT = new hydra.core.Name("increment");
  
  public static final hydra.core.Name FIELD_NAME_DECREMENT = new hydra.core.Name("decrement");
  
  public static final hydra.core.Name FIELD_NAME_TRUE = new hydra.core.Name("true");
  
  public static final hydra.core.Name FIELD_NAME_FALSE = new hydra.core.Name("false");
  
  private OverloadableUnaryOperator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Plus instance) ;
    
    R visit(Minus instance) ;
    
    R visit(Not instance) ;
    
    R visit(Complement instance) ;
    
    R visit(Increment instance) ;
    
    R visit(Decrement instance) ;
    
    R visit(True instance) ;
    
    R visit(False instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OverloadableUnaryOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Plus instance) {
      return otherwise((instance));
    }
    
    default R visit(Minus instance) {
      return otherwise((instance));
    }
    
    default R visit(Not instance) {
      return otherwise((instance));
    }
    
    default R visit(Complement instance) {
      return otherwise((instance));
    }
    
    default R visit(Increment instance) {
      return otherwise((instance));
    }
    
    default R visit(Decrement instance) {
      return otherwise((instance));
    }
    
    default R visit(True instance) {
      return otherwise((instance));
    }
    
    default R visit(False instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Plus extends hydra.ext.csharp.syntax.OverloadableUnaryOperator implements Serializable {
    public Plus () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Plus)) {
        return false;
      }
      Plus o = (Plus) (other);
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
  
  public static final class Minus extends hydra.ext.csharp.syntax.OverloadableUnaryOperator implements Serializable {
    public Minus () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Minus)) {
        return false;
      }
      Minus o = (Minus) (other);
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
  
  public static final class Not extends hydra.ext.csharp.syntax.OverloadableUnaryOperator implements Serializable {
    public Not () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Not)) {
        return false;
      }
      Not o = (Not) (other);
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
  
  public static final class Complement extends hydra.ext.csharp.syntax.OverloadableUnaryOperator implements Serializable {
    public Complement () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Complement)) {
        return false;
      }
      Complement o = (Complement) (other);
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
  
  public static final class Increment extends hydra.ext.csharp.syntax.OverloadableUnaryOperator implements Serializable {
    public Increment () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Increment)) {
        return false;
      }
      Increment o = (Increment) (other);
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
  
  public static final class Decrement extends hydra.ext.csharp.syntax.OverloadableUnaryOperator implements Serializable {
    public Decrement () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Decrement)) {
        return false;
      }
      Decrement o = (Decrement) (other);
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
  
  public static final class True extends hydra.ext.csharp.syntax.OverloadableUnaryOperator implements Serializable {
    public True () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof True)) {
        return false;
      }
      True o = (True) (other);
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
  
  public static final class False extends hydra.ext.csharp.syntax.OverloadableUnaryOperator implements Serializable {
    public False () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof False)) {
        return false;
      }
      False o = (False) (other);
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