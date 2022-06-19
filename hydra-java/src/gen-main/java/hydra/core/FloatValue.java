package hydra.core;

/**
 * A floating-point literal value
 */
public abstract class FloatValue {
  private FloatValue () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Bigfloat instance) ;
    
    R visit(Float32 instance) ;
    
    R visit(Float64 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(FloatValue instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Bigfloat instance) {
      return otherwise((instance));
    }
    
    default R visit(Float32 instance) {
      return otherwise((instance));
    }
    
    default R visit(Float64 instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Bigfloat extends FloatValue {
    public final Double value;
    
    public Bigfloat (Double value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bigfloat)) {
        return false;
      }
      Bigfloat o = (Bigfloat) (other);
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
  
  /**
   * A 32-bit floating-point value
   */
  public static final class Float32 extends FloatValue {
    /**
     * A 32-bit floating-point value
     */
    public final Float value;
    
    public Float32 (Float value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Float32)) {
        return false;
      }
      Float32 o = (Float32) (other);
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
  
  /**
   * A 64-bit floating-point value
   */
  public static final class Float64 extends FloatValue {
    /**
     * A 64-bit floating-point value
     */
    public final Double value;
    
    public Float64 (Double value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Float64)) {
        return false;
      }
      Float64 o = (Float64) (other);
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