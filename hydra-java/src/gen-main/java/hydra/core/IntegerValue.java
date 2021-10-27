package hydra.core;

public abstract class IntegerValue {
  private IntegerValue() {}
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  /**
   * An interface for applying a function to a IntegerValue according to its variant (subclass)
   */
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
  
  /**
   * An interface for applying a function to a IntegerValue according to its variant (subclass). If a visit() method for a
   * particular variant is not implemented, a default method is used instead.
   */
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(IntegerValue instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    @Override
    default R visit(Bigint instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Int8 instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Int16 instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Int32 instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Int64 instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Uint8 instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Uint16 instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Uint32 instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Uint64 instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Bigint extends IntegerValue {
    public final Long bigint;
    
    /**
     * Constructs an immutable Bigint object
     */
    public Bigint(Long bigint) {
      this.bigint = bigint;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bigint)) {
          return false;
      }
      Bigint o = (Bigint) other;
      return bigint.equals(o.bigint);
    }
    
    @Override
    public int hashCode() {
      return 2 * bigint.hashCode();
    }
  }
  
  public static final class Int8 extends IntegerValue {
    public final Byte int8;
    
    /**
     * Constructs an immutable Int8 object
     */
    public Int8(Byte int8) {
      this.int8 = int8;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int8)) {
          return false;
      }
      Int8 o = (Int8) other;
      return int8.equals(o.int8);
    }
    
    @Override
    public int hashCode() {
      return 2 * int8.hashCode();
    }
  }
  
  public static final class Int16 extends IntegerValue {
    public final Short int16;
    
    /**
     * Constructs an immutable Int16 object
     */
    public Int16(Short int16) {
      this.int16 = int16;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int16)) {
          return false;
      }
      Int16 o = (Int16) other;
      return int16.equals(o.int16);
    }
    
    @Override
    public int hashCode() {
      return 2 * int16.hashCode();
    }
  }
  
  public static final class Int32 extends IntegerValue {
    public final Integer int32;
    
    /**
     * Constructs an immutable Int32 object
     */
    public Int32(Integer int32) {
      this.int32 = int32;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int32)) {
          return false;
      }
      Int32 o = (Int32) other;
      return int32.equals(o.int32);
    }
    
    @Override
    public int hashCode() {
      return 2 * int32.hashCode();
    }
  }
  
  public static final class Int64 extends IntegerValue {
    public final Long int64;
    
    /**
     * Constructs an immutable Int64 object
     */
    public Int64(Long int64) {
      this.int64 = int64;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int64)) {
          return false;
      }
      Int64 o = (Int64) other;
      return int64.equals(o.int64);
    }
    
    @Override
    public int hashCode() {
      return 2 * int64.hashCode();
    }
  }
  
  public static final class Uint8 extends IntegerValue {
    public final Byte uint8;
    
    /**
     * Constructs an immutable Uint8 object
     */
    public Uint8(Byte uint8) {
      this.uint8 = uint8;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint8)) {
          return false;
      }
      Uint8 o = (Uint8) other;
      return uint8.equals(o.uint8);
    }
    
    @Override
    public int hashCode() {
      return 2 * uint8.hashCode();
    }
  }
  
  public static final class Uint16 extends IntegerValue {
    public final Short uint16;
    
    /**
     * Constructs an immutable Uint16 object
     */
    public Uint16(Short uint16) {
      this.uint16 = uint16;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint16)) {
          return false;
      }
      Uint16 o = (Uint16) other;
      return uint16.equals(o.uint16);
    }
    
    @Override
    public int hashCode() {
      return 2 * uint16.hashCode();
    }
  }
  
  public static final class Uint32 extends IntegerValue {
    public final Integer uint32;
    
    /**
     * Constructs an immutable Uint32 object
     */
    public Uint32(Integer uint32) {
      this.uint32 = uint32;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint32)) {
          return false;
      }
      Uint32 o = (Uint32) other;
      return uint32.equals(o.uint32);
    }
    
    @Override
    public int hashCode() {
      return 2 * uint32.hashCode();
    }
  }
  
  public static final class Uint64 extends IntegerValue {
    public final Long uint64;
    
    /**
     * Constructs an immutable Uint64 object
     */
    public Uint64(Long uint64) {
      this.uint64 = uint64;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint64)) {
          return false;
      }
      Uint64 o = (Uint64) other;
      return uint64.equals(o.uint64);
    }
    
    @Override
    public int hashCode() {
      return 2 * uint64.hashCode();
    }
  }
}
