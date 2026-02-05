// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * An integer literal value
 */
public abstract class IntegerValue implements Serializable, Comparable<IntegerValue> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.IntegerValue");
  
  public static final hydra.core.Name FIELD_NAME_BIGINT = new hydra.core.Name("bigint");
  
  public static final hydra.core.Name FIELD_NAME_INT8 = new hydra.core.Name("int8");
  
  public static final hydra.core.Name FIELD_NAME_INT16 = new hydra.core.Name("int16");
  
  public static final hydra.core.Name FIELD_NAME_INT32 = new hydra.core.Name("int32");
  
  public static final hydra.core.Name FIELD_NAME_INT64 = new hydra.core.Name("int64");
  
  public static final hydra.core.Name FIELD_NAME_UINT8 = new hydra.core.Name("uint8");
  
  public static final hydra.core.Name FIELD_NAME_UINT16 = new hydra.core.Name("uint16");
  
  public static final hydra.core.Name FIELD_NAME_UINT32 = new hydra.core.Name("uint32");
  
  public static final hydra.core.Name FIELD_NAME_UINT64 = new hydra.core.Name("uint64");
  
  private IntegerValue () {
  
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
    default R otherwise(IntegerValue instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Bigint instance) {
      return otherwise((instance));
    }
    
    default R visit(Int8 instance) {
      return otherwise((instance));
    }
    
    default R visit(Int16 instance) {
      return otherwise((instance));
    }
    
    default R visit(Int32 instance) {
      return otherwise((instance));
    }
    
    default R visit(Int64 instance) {
      return otherwise((instance));
    }
    
    default R visit(Uint8 instance) {
      return otherwise((instance));
    }
    
    default R visit(Uint16 instance) {
      return otherwise((instance));
    }
    
    default R visit(Uint32 instance) {
      return otherwise((instance));
    }
    
    default R visit(Uint64 instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * An arbitrary-precision integer value
   */
  public static final class Bigint extends hydra.core.IntegerValue implements Serializable {
    public final java.math.BigInteger value;
    
    public Bigint (java.math.BigInteger value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bigint)) {
        return false;
      }
      Bigint o = (Bigint) (other);
      return this.value.compareTo(o.value) == 0;
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegerValue other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Bigint o = (Bigint) (other);
      return ((Comparable) (value)).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * An 8-bit signed integer value
   */
  public static final class Int8 extends hydra.core.IntegerValue implements Serializable {
    public final Byte value;
    
    public Int8 (Byte value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int8)) {
        return false;
      }
      Int8 o = (Int8) (other);
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegerValue other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Int8 o = (Int8) (other);
      return ((Comparable) (value)).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A 16-bit signed integer value (short value)
   */
  public static final class Int16 extends hydra.core.IntegerValue implements Serializable {
    public final Short value;
    
    public Int16 (Short value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int16)) {
        return false;
      }
      Int16 o = (Int16) (other);
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegerValue other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Int16 o = (Int16) (other);
      return ((Comparable) (value)).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A 32-bit signed integer value (int value)
   */
  public static final class Int32 extends hydra.core.IntegerValue implements Serializable {
    public final Integer value;
    
    public Int32 (Integer value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int32)) {
        return false;
      }
      Int32 o = (Int32) (other);
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegerValue other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Int32 o = (Int32) (other);
      return ((Comparable) (value)).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A 64-bit signed integer value (long value)
   */
  public static final class Int64 extends hydra.core.IntegerValue implements Serializable {
    public final Long value;
    
    public Int64 (Long value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int64)) {
        return false;
      }
      Int64 o = (Int64) (other);
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegerValue other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Int64 o = (Int64) (other);
      return ((Comparable) (value)).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * An 8-bit unsigned integer value (byte)
   */
  public static final class Uint8 extends hydra.core.IntegerValue implements Serializable {
    public final Short value;
    
    public Uint8 (Short value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint8)) {
        return false;
      }
      Uint8 o = (Uint8) (other);
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegerValue other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Uint8 o = (Uint8) (other);
      return ((Comparable) (value)).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A 16-bit unsigned integer value
   */
  public static final class Uint16 extends hydra.core.IntegerValue implements Serializable {
    public final Character value;
    
    public Uint16 (Character value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint16)) {
        return false;
      }
      Uint16 o = (Uint16) (other);
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegerValue other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Uint16 o = (Uint16) (other);
      return ((Comparable) (value)).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A 32-bit unsigned integer value (unsigned int)
   */
  public static final class Uint32 extends hydra.core.IntegerValue implements Serializable {
    public final Long value;
    
    public Uint32 (Long value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint32)) {
        return false;
      }
      Uint32 o = (Uint32) (other);
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegerValue other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Uint32 o = (Uint32) (other);
      return ((Comparable) (value)).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A 64-bit unsigned integer value (unsigned long)
   */
  public static final class Uint64 extends hydra.core.IntegerValue implements Serializable {
    public final java.math.BigInteger value;
    
    public Uint64 (java.math.BigInteger value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint64)) {
        return false;
      }
      Uint64 o = (Uint64) (other);
      return this.value.compareTo(o.value) == 0;
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(IntegerValue other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Uint64 o = (Uint64) (other);
      return ((Comparable) (value)).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
