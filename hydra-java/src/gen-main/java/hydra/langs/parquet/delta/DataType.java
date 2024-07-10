// Note: this is an automatically generated file. Do not edit.

package hydra.langs.parquet.delta;

import java.io.Serializable;

public abstract class DataType implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/parquet/delta.DataType");
  
  private DataType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Array instance) ;
    
    R visit(Binary instance) ;
    
    R visit(Boolean_ instance) ;
    
    R visit(Byte_ instance) ;
    
    R visit(Date instance) ;
    
    R visit(Decimal instance) ;
    
    R visit(Double_ instance) ;
    
    R visit(Float_ instance) ;
    
    R visit(Integer_ instance) ;
    
    R visit(Long_ instance) ;
    
    R visit(Map instance) ;
    
    R visit(Null instance) ;
    
    R visit(Short_ instance) ;
    
    R visit(String_ instance) ;
    
    R visit(Struct instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DataType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Array instance) {
      return otherwise((instance));
    }
    
    default R visit(Binary instance) {
      return otherwise((instance));
    }
    
    default R visit(Boolean_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Byte_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Date instance) {
      return otherwise((instance));
    }
    
    default R visit(Decimal instance) {
      return otherwise((instance));
    }
    
    default R visit(Double_ instance) {
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
    
    default R visit(Map instance) {
      return otherwise((instance));
    }
    
    default R visit(Null instance) {
      return otherwise((instance));
    }
    
    default R visit(Short_ instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Struct instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Array extends hydra.langs.parquet.delta.DataType implements Serializable {
    public final hydra.langs.parquet.delta.ArrayType value;
    
    public Array (hydra.langs.parquet.delta.ArrayType value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Array)) {
        return false;
      }
      Array o = (Array) (other);
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
  
  public static final class Binary extends hydra.langs.parquet.delta.DataType implements Serializable {
    public Binary () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Binary)) {
        return false;
      }
      Binary o = (Binary) (other);
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
  
  public static final class Boolean_ extends hydra.langs.parquet.delta.DataType implements Serializable {
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
  
  public static final class Byte_ extends hydra.langs.parquet.delta.DataType implements Serializable {
    public Byte_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Byte_)) {
        return false;
      }
      Byte_ o = (Byte_) (other);
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
  
  public static final class Date extends hydra.langs.parquet.delta.DataType implements Serializable {
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
  
  public static final class Decimal extends hydra.langs.parquet.delta.DataType implements Serializable {
    public final hydra.langs.parquet.delta.DecimalType value;
    
    public Decimal (hydra.langs.parquet.delta.DecimalType value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Decimal)) {
        return false;
      }
      Decimal o = (Decimal) (other);
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
  
  public static final class Double_ extends hydra.langs.parquet.delta.DataType implements Serializable {
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
  
  public static final class Float_ extends hydra.langs.parquet.delta.DataType implements Serializable {
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
  
  public static final class Integer_ extends hydra.langs.parquet.delta.DataType implements Serializable {
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
  
  public static final class Long_ extends hydra.langs.parquet.delta.DataType implements Serializable {
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
  
  public static final class Map extends hydra.langs.parquet.delta.DataType implements Serializable {
    public final hydra.langs.parquet.delta.MapType value;
    
    public Map (hydra.langs.parquet.delta.MapType value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
        return false;
      }
      Map o = (Map) (other);
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
  
  public static final class Null extends hydra.langs.parquet.delta.DataType implements Serializable {
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
  
  public static final class Short_ extends hydra.langs.parquet.delta.DataType implements Serializable {
    public Short_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Short_)) {
        return false;
      }
      Short_ o = (Short_) (other);
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
  
  public static final class String_ extends hydra.langs.parquet.delta.DataType implements Serializable {
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
  
  public static final class Struct extends hydra.langs.parquet.delta.DataType implements Serializable {
    public final hydra.langs.parquet.delta.StructType value;
    
    public Struct (hydra.langs.parquet.delta.StructType value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Struct)) {
        return false;
      }
      Struct o = (Struct) (other);
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