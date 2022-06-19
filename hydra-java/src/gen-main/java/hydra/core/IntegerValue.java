package hydra.core;

/**
 * An integer literal value
 */
public abstract class IntegerValue {
  private IntegerValue () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Bigint instance) ;
    
    R visit(Int16 instance) ;
    
    R visit(Int32 instance) ;
    
    R visit(Int64 instance) ;
    
    R visit(Int8 instance) ;
    
    R visit(Uint16 instance) ;
    
    R visit(Uint32 instance) ;
    
    R visit(Uint64 instance) ;
    
    R visit(Uint8 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(IntegerValue instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Bigint instance) {
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
    
    default R visit(Int8 instance) {
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
    
    default R visit(Uint8 instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Bigint extends IntegerValue {
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
  
  public static final class Int16 extends IntegerValue {
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
  
  public static final class Int32 extends IntegerValue {
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
  
  public static final class Int64 extends IntegerValue {
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
  
  public static final class Int8 extends IntegerValue {
    public final Short value;
    
    public Int8 (Short value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int8)) {
        return false;
      }
      Int8 o = (Int8) (other);
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
  
  public static final class Uint16 extends IntegerValue {
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
  
  public static final class Uint32 extends IntegerValue {
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
  
  public static final class Uint64 extends IntegerValue {
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
  
  public static final class Uint8 extends IntegerValue {
    public final Byte value;
    
    public Uint8 (Byte value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uint8)) {
        return false;
      }
      Uint8 o = (Uint8) (other);
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