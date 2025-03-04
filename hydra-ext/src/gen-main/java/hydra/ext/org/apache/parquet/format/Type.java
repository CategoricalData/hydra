// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.parquet.format;

import java.io.Serializable;

/**
 * Types supported by Parquet.  These types are intended to be used in combination with the encodings to control the on disk storage format. For example INT16 is not included as a type since a good encoding of INT32 would handle this.
 */
public abstract class Type implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.apache.parquet.format.Type");
  
  public static final hydra.core.Name FIELD_NAME_BOOLEAN = new hydra.core.Name("boolean");
  
  public static final hydra.core.Name FIELD_NAME_INT32 = new hydra.core.Name("int32");
  
  public static final hydra.core.Name FIELD_NAME_INT64 = new hydra.core.Name("int64");
  
  public static final hydra.core.Name FIELD_NAME_FLOAT = new hydra.core.Name("float");
  
  public static final hydra.core.Name FIELD_NAME_DOUBLE = new hydra.core.Name("double");
  
  public static final hydra.core.Name FIELD_NAME_BYTE_ARRAY = new hydra.core.Name("byteArray");
  
  public static final hydra.core.Name FIELD_NAME_FIXED_LEN_BYTE_ARRAY = new hydra.core.Name("fixedLenByteArray");
  
  private Type () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Boolean_ instance) ;
    
    R visit(Int32 instance) ;
    
    R visit(Int64 instance) ;
    
    R visit(Float_ instance) ;
    
    R visit(Double_ instance) ;
    
    R visit(ByteArray instance) ;
    
    R visit(FixedLenByteArray instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Type instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Boolean_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Int32 instance) {
      return otherwise((instance));
    }
    
    default R visit(Int64 instance) {
      return otherwise((instance));
    }
    
    default R visit(Float_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Double_ instance) {
      return otherwise((instance));
    }
    
    default R visit(ByteArray instance) {
      return otherwise((instance));
    }
    
    default R visit(FixedLenByteArray instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Boolean_ extends hydra.ext.org.apache.parquet.format.Type implements Serializable {
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
  
  public static final class Int32 extends hydra.ext.org.apache.parquet.format.Type implements Serializable {
    public Int32 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int32)) {
        return false;
      }
      Int32 o = (Int32) (other);
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
  
  public static final class Int64 extends hydra.ext.org.apache.parquet.format.Type implements Serializable {
    public Int64 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int64)) {
        return false;
      }
      Int64 o = (Int64) (other);
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
  
  public static final class Float_ extends hydra.ext.org.apache.parquet.format.Type implements Serializable {
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
  
  public static final class Double_ extends hydra.ext.org.apache.parquet.format.Type implements Serializable {
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
  
  public static final class ByteArray extends hydra.ext.org.apache.parquet.format.Type implements Serializable {
    public ByteArray () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ByteArray)) {
        return false;
      }
      ByteArray o = (ByteArray) (other);
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
  
  public static final class FixedLenByteArray extends hydra.ext.org.apache.parquet.format.Type implements Serializable {
    public FixedLenByteArray () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FixedLenByteArray)) {
        return false;
      }
      FixedLenByteArray o = (FixedLenByteArray) (other);
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