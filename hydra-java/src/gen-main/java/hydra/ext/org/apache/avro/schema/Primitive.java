// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.avro.schema;

import java.io.Serializable;

public abstract class Primitive implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.avro.schema.Primitive");
  
  public static final hydra.core.Name FIELD_NAME_NULL = new hydra.core.Name("null");
  
  public static final hydra.core.Name FIELD_NAME_BOOLEAN = new hydra.core.Name("boolean");
  
  public static final hydra.core.Name FIELD_NAME_INT = new hydra.core.Name("int");
  
  public static final hydra.core.Name FIELD_NAME_LONG = new hydra.core.Name("long");
  
  public static final hydra.core.Name FIELD_NAME_FLOAT = new hydra.core.Name("float");
  
  public static final hydra.core.Name FIELD_NAME_DOUBLE = new hydra.core.Name("double");
  
  public static final hydra.core.Name FIELD_NAME_BYTES = new hydra.core.Name("bytes");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  private Primitive () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Null instance) ;
    
    R visit(Boolean_ instance) ;
    
    R visit(Int instance) ;
    
    R visit(Long_ instance) ;
    
    R visit(Float_ instance) ;
    
    R visit(Double_ instance) ;
    
    R visit(Bytes instance) ;
    
    R visit(String_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Primitive instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Null instance) {
      return otherwise((instance));
    }
    
    default R visit(Boolean_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Int instance) {
      return otherwise((instance));
    }
    
    default R visit(Long_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Float_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Double_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Bytes instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * no value
   */
  public static final class Null extends hydra.ext.org.apache.avro.schema.Primitive implements Serializable {
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
   * A binary value
   */
  public static final class Boolean_ extends hydra.ext.org.apache.avro.schema.Primitive implements Serializable {
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
   * 32-bit signed integer
   */
  public static final class Int extends hydra.ext.org.apache.avro.schema.Primitive implements Serializable {
    public Int () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int)) {
        return false;
      }
      Int o = (Int) (other);
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
   * 64-bit signed integer
   */
  public static final class Long_ extends hydra.ext.org.apache.avro.schema.Primitive implements Serializable {
    public Long_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Long_)) {
        return false;
      }
      Long_ o = (Long_) (other);
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
   * single precision (32-bit) IEEE 754 floating-point number
   */
  public static final class Float_ extends hydra.ext.org.apache.avro.schema.Primitive implements Serializable {
    public Float_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Float_)) {
        return false;
      }
      Float_ o = (Float_) (other);
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
   * double precision (64-bit) IEEE 754 floating-point number
   */
  public static final class Double_ extends hydra.ext.org.apache.avro.schema.Primitive implements Serializable {
    public Double_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Double_)) {
        return false;
      }
      Double_ o = (Double_) (other);
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
   * sequence of 8-bit unsigned bytes
   */
  public static final class Bytes extends hydra.ext.org.apache.avro.schema.Primitive implements Serializable {
    public Bytes () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bytes)) {
        return false;
      }
      Bytes o = (Bytes) (other);
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
   * unicode character sequence
   */
  public static final class String_ extends hydra.ext.org.apache.avro.schema.Primitive implements Serializable {
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
}