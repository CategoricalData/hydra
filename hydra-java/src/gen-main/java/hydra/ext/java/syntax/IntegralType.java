// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class IntegralType implements Serializable, Comparable<IntegralType> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.IntegralType");
  
  public static final hydra.core.Name FIELD_NAME_BYTE = new hydra.core.Name("byte");
  
  public static final hydra.core.Name FIELD_NAME_SHORT = new hydra.core.Name("short");
  
  public static final hydra.core.Name FIELD_NAME_INT = new hydra.core.Name("int");
  
  public static final hydra.core.Name FIELD_NAME_LONG = new hydra.core.Name("long");
  
  public static final hydra.core.Name FIELD_NAME_CHAR = new hydra.core.Name("char");
  
  private IntegralType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Byte_ instance) ;
    
    R visit(Short_ instance) ;
    
    R visit(Int instance) ;
    
    R visit(Long_ instance) ;
    
    R visit(Char instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(IntegralType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Byte_ instance) {
      return otherwise(instance);
    }
    
    default R visit(Short_ instance) {
      return otherwise(instance);
    }
    
    default R visit(Int instance) {
      return otherwise(instance);
    }
    
    default R visit(Long_ instance) {
      return otherwise(instance);
    }
    
    default R visit(Char instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Byte_ extends hydra.ext.java.syntax.IntegralType implements Serializable {
    public Byte_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Byte_)) {
        return false;
      }
      Byte_ o = (Byte_) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegralType other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Short_ extends hydra.ext.java.syntax.IntegralType implements Serializable {
    public Short_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Short_)) {
        return false;
      }
      Short_ o = (Short_) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegralType other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Int extends hydra.ext.java.syntax.IntegralType implements Serializable {
    public Int () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int)) {
        return false;
      }
      Int o = (Int) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegralType other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Long_ extends hydra.ext.java.syntax.IntegralType implements Serializable {
    public Long_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Long_)) {
        return false;
      }
      Long_ o = (Long_) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegralType other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Char extends hydra.ext.java.syntax.IntegralType implements Serializable {
    public Char () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Char)) {
        return false;
      }
      Char o = (Char) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegralType other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
