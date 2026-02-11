// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * An integer type
 */
public abstract class IntegerType implements Serializable, Comparable<IntegerType> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.IntegerType");
  
  public static final hydra.core.Name FIELD_NAME_BIGINT = new hydra.core.Name("bigint");
  
  public static final hydra.core.Name FIELD_NAME_INT8 = new hydra.core.Name("int8");
  
  public static final hydra.core.Name FIELD_NAME_INT16 = new hydra.core.Name("int16");
  
  public static final hydra.core.Name FIELD_NAME_INT32 = new hydra.core.Name("int32");
  
  public static final hydra.core.Name FIELD_NAME_INT64 = new hydra.core.Name("int64");
  
  public static final hydra.core.Name FIELD_NAME_UINT8 = new hydra.core.Name("uint8");
  
  public static final hydra.core.Name FIELD_NAME_UINT16 = new hydra.core.Name("uint16");
  
  public static final hydra.core.Name FIELD_NAME_UINT32 = new hydra.core.Name("uint32");
  
  public static final hydra.core.Name FIELD_NAME_UINT64 = new hydra.core.Name("uint64");
  
  private IntegerType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Bigint instance) ;
    
    R visit(Int8 instance) ;
    
    R visit(Int16 instance) ;
    
    R visit(Int32 instance) ;
    
    R visit(Int64 instance) ;
    
    R visit(Uint8 instance) ;
    
    R visit(Uint16 instance) ;
    
    R visit(Uint32 instance) ;
    
    R visit(Uint64 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(IntegerType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Bigint instance) {
      return otherwise(instance);
    }
    
    default R visit(Int8 instance) {
      return otherwise(instance);
    }
    
    default R visit(Int16 instance) {
      return otherwise(instance);
    }
    
    default R visit(Int32 instance) {
      return otherwise(instance);
    }
    
    default R visit(Int64 instance) {
      return otherwise(instance);
    }
    
    default R visit(Uint8 instance) {
      return otherwise(instance);
    }
    
    default R visit(Uint16 instance) {
      return otherwise(instance);
    }
    
    default R visit(Uint32 instance) {
      return otherwise(instance);
    }
    
    default R visit(Uint64 instance) {
      return otherwise(instance);
    }
  }
  
  /**
   * An arbitrary-precision integer type
   */
  public static final class Bigint extends hydra.core.IntegerType implements Serializable {
    public Bigint () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bigint)) {
        return false;
      }
      Bigint o = (Bigint) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegerType other) {
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
  
  /**
   * An 8-bit signed integer type
   */
  public static final class Int8 extends hydra.core.IntegerType implements Serializable {
    public Int8 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int8)) {
        return false;
      }
      Int8 o = (Int8) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegerType other) {
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
  
  /**
   * A 16-bit signed integer type
   */
  public static final class Int16 extends hydra.core.IntegerType implements Serializable {
    public Int16 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int16)) {
        return false;
      }
      Int16 o = (Int16) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegerType other) {
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
  
  /**
   * A 32-bit signed integer type
   */
  public static final class Int32 extends hydra.core.IntegerType implements Serializable {
    public Int32 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int32)) {
        return false;
      }
      Int32 o = (Int32) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegerType other) {
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
  
  /**
   * A 64-bit signed integer type
   */
  public static final class Int64 extends hydra.core.IntegerType implements Serializable {
    public Int64 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int64)) {
        return false;
      }
      Int64 o = (Int64) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegerType other) {
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
  
  /**
   * An 8-bit unsigned integer type
   */
  public static final class Uint8 extends hydra.core.IntegerType implements Serializable {
    public Uint8 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint8)) {
        return false;
      }
      Uint8 o = (Uint8) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegerType other) {
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
  
  /**
   * A 16-bit unsigned integer type
   */
  public static final class Uint16 extends hydra.core.IntegerType implements Serializable {
    public Uint16 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint16)) {
        return false;
      }
      Uint16 o = (Uint16) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegerType other) {
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
  
  /**
   * A 32-bit unsigned integer type
   */
  public static final class Uint32 extends hydra.core.IntegerType implements Serializable {
    public Uint32 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint32)) {
        return false;
      }
      Uint32 o = (Uint32) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegerType other) {
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
  
  /**
   * A 64-bit unsigned integer type
   */
  public static final class Uint64 extends hydra.core.IntegerType implements Serializable {
    public Uint64 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint64)) {
        return false;
      }
      Uint64 o = (Uint64) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegerType other) {
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
