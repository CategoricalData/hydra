// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.azure.dtld;

import java.io.Serializable;

/**
 * A full set of primitive data types are provided and can be specified directly as the value in a schema statement in a digital twin interface.
 */
public abstract class Schema_Primitive implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.com.microsoft.azure.dtld.Schema_Primitive");
  
  public static final hydra.core.Name FIELD_NAME_BOOLEAN = new hydra.core.Name("boolean");
  
  public static final hydra.core.Name FIELD_NAME_DATE = new hydra.core.Name("date");
  
  public static final hydra.core.Name FIELD_NAME_DATE_TIME = new hydra.core.Name("dateTime");
  
  public static final hydra.core.Name FIELD_NAME_DOUBLE = new hydra.core.Name("double");
  
  public static final hydra.core.Name FIELD_NAME_DURATION = new hydra.core.Name("duration");
  
  public static final hydra.core.Name FIELD_NAME_FLOAT = new hydra.core.Name("float");
  
  public static final hydra.core.Name FIELD_NAME_INTEGER = new hydra.core.Name("integer");
  
  public static final hydra.core.Name FIELD_NAME_LONG = new hydra.core.Name("long");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name FIELD_NAME_TIME = new hydra.core.Name("time");
  
  private Schema_Primitive () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Boolean_ instance) ;
    
    R visit(Date instance) ;
    
    R visit(DateTime instance) ;
    
    R visit(Double_ instance) ;
    
    R visit(Duration instance) ;
    
    R visit(Float_ instance) ;
    
    R visit(Integer_ instance) ;
    
    R visit(Long_ instance) ;
    
    R visit(String_ instance) ;
    
    R visit(Time instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Schema_Primitive instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Boolean_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Date instance) {
      return otherwise((instance));
    }
    
    default R visit(DateTime instance) {
      return otherwise((instance));
    }
    
    default R visit(Double_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Duration instance) {
      return otherwise((instance));
    }
    
    default R visit(Float_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Integer_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Long_ instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Time instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * A boolean value
   */
  public static final class Boolean_ extends hydra.ext.com.microsoft.azure.dtld.Schema_Primitive implements Serializable {
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
   * A full-date as defined in section 5.6 of RFC 3339
   */
  public static final class Date extends hydra.ext.com.microsoft.azure.dtld.Schema_Primitive implements Serializable {
    public Date () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Date)) {
        return false;
      }
      Date o = (Date) (other);
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
   * A date-time as defined in RFC 3339
   */
  public static final class DateTime extends hydra.ext.com.microsoft.azure.dtld.Schema_Primitive implements Serializable {
    public DateTime () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DateTime)) {
        return false;
      }
      DateTime o = (DateTime) (other);
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
   * An IEEE 8-byte floating point
   */
  public static final class Double_ extends hydra.ext.com.microsoft.azure.dtld.Schema_Primitive implements Serializable {
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
   * A duration in ISO 8601 format
   */
  public static final class Duration extends hydra.ext.com.microsoft.azure.dtld.Schema_Primitive implements Serializable {
    public Duration () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Duration)) {
        return false;
      }
      Duration o = (Duration) (other);
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
   * An IEEE 4-byte floating point
   */
  public static final class Float_ extends hydra.ext.com.microsoft.azure.dtld.Schema_Primitive implements Serializable {
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
   * A signed 4-byte integer
   */
  public static final class Integer_ extends hydra.ext.com.microsoft.azure.dtld.Schema_Primitive implements Serializable {
    public Integer_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Integer_)) {
        return false;
      }
      Integer_ o = (Integer_) (other);
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
   * A signed 8-byte integer
   */
  public static final class Long_ extends hydra.ext.com.microsoft.azure.dtld.Schema_Primitive implements Serializable {
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
   * A UTF8 string
   */
  public static final class String_ extends hydra.ext.com.microsoft.azure.dtld.Schema_Primitive implements Serializable {
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
   * A full-time as defined in section 5.6 of RFC 3339
   */
  public static final class Time extends hydra.ext.com.microsoft.azure.dtld.Schema_Primitive implements Serializable {
    public Time () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Time)) {
        return false;
      }
      Time o = (Time) (other);
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