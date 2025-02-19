// Note: this is an automatically generated file. Do not edit.

package hydra.ext.typeScript.model;

import java.io.Serializable;

public abstract class PrimitiveType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.typeScript.model.PrimitiveType");
  
  public static final hydra.core.Name FIELD_NAME_BIGINT = new hydra.core.Name("bigint");
  
  public static final hydra.core.Name FIELD_NAME_BOOLEAN = new hydra.core.Name("boolean");
  
  public static final hydra.core.Name FIELD_NAME_NULL = new hydra.core.Name("null");
  
  public static final hydra.core.Name FIELD_NAME_NUMBER = new hydra.core.Name("number");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT = new hydra.core.Name("object");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name FIELD_NAME_SYMBOL = new hydra.core.Name("symbol");
  
  public static final hydra.core.Name FIELD_NAME_UNDEFINED = new hydra.core.Name("undefined");
  
  private PrimitiveType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Bigint instance) ;
    
    R visit(Boolean_ instance) ;
    
    R visit(Null instance) ;
    
    R visit(Number_ instance) ;
    
    R visit(Object_ instance) ;
    
    R visit(String_ instance) ;
    
    R visit(Symbol instance) ;
    
    R visit(Undefined instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PrimitiveType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Bigint instance) {
      return otherwise((instance));
    }
    
    default R visit(Boolean_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Null instance) {
      return otherwise((instance));
    }
    
    default R visit(Number_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Object_ instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Symbol instance) {
      return otherwise((instance));
    }
    
    default R visit(Undefined instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * integers in the arbitrary precision format
   */
  public static final class Bigint extends hydra.ext.typeScript.model.PrimitiveType implements Serializable {
    public Bigint () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bigint)) {
        return false;
      }
      Bigint o = (Bigint) (other);
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
  
  /**
   * true and false
   */
  public static final class Boolean_ extends hydra.ext.typeScript.model.PrimitiveType implements Serializable {
    public Boolean_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Boolean_)) {
        return false;
      }
      Boolean_ o = (Boolean_) (other);
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
  
  /**
   * equivalent to the unit type
   */
  public static final class Null extends hydra.ext.typeScript.model.PrimitiveType implements Serializable {
    public Null () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Null)) {
        return false;
      }
      Null o = (Null) (other);
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
  
  /**
   * a double-precision IEEE 754 floating point
   */
  public static final class Number_ extends hydra.ext.typeScript.model.PrimitiveType implements Serializable {
    public Number_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Number_)) {
        return false;
      }
      Number_ o = (Number_) (other);
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
  
  /**
   * similar to records
   */
  public static final class Object_ extends hydra.ext.typeScript.model.PrimitiveType implements Serializable {
    public Object_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Object_)) {
        return false;
      }
      Object_ o = (Object_) (other);
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
  
  /**
   * an immutable UTF-16 string
   */
  public static final class String_ extends hydra.ext.typeScript.model.PrimitiveType implements Serializable {
    public String_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) (other);
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
  
  /**
   * a unique value usually used as a key
   */
  public static final class Symbol extends hydra.ext.typeScript.model.PrimitiveType implements Serializable {
    public Symbol () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Symbol)) {
        return false;
      }
      Symbol o = (Symbol) (other);
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
  
  /**
   * also equivalent to the unit type
   */
  public static final class Undefined extends hydra.ext.typeScript.model.PrimitiveType implements Serializable {
    public Undefined () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Undefined)) {
        return false;
      }
      Undefined o = (Undefined) (other);
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