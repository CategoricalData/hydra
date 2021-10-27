package hydra.core;

public abstract class FloatValue {
  private FloatValue() {}
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  /**
   * An interface for applying a function to a FloatValue according to its variant (subclass)
   */
  public interface Visitor<R> {
    R visit(Bigfloat instance) ;
    
    R visit(Float32 instance) ;
    
    R visit(Float64 instance) ;
  }
  
  /**
   * An interface for applying a function to a FloatValue according to its variant (subclass). If a visit() method for a
   * particular variant is not implemented, a default method is used instead.
   */
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(FloatValue instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    @Override
    default R visit(Bigfloat instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Float32 instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Float64 instance) {
      return otherwise(instance);
    }
  }
  
  /**
   * @type float:
   *         precision: arbitrary
   */
  public static final class Bigfloat extends FloatValue {
    public final Double bigfloat;
    
    /**
     * Constructs an immutable Bigfloat object
     */
    public Bigfloat(Double bigfloat) {
      this.bigfloat = bigfloat;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bigfloat)) {
          return false;
      }
      Bigfloat o = (Bigfloat) other;
      return bigfloat.equals(o.bigfloat);
    }
    
    @Override
    public int hashCode() {
      return 2 * bigfloat.hashCode();
    }
  }
  
  /**
   * @type float
   */
  public static final class Float32 extends FloatValue {
    public final Float float32;
    
    /**
     * Constructs an immutable Float32 object
     */
    public Float32(Float float32) {
      this.float32 = float32;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Float32)) {
          return false;
      }
      Float32 o = (Float32) other;
      return float32.equals(o.float32);
    }
    
    @Override
    public int hashCode() {
      return 2 * float32.hashCode();
    }
  }
  
  /**
   * @type float:
   *         precision:
   *           bits: 64
   */
  public static final class Float64 extends FloatValue {
    public final Double float64;
    
    /**
     * Constructs an immutable Float64 object
     */
    public Float64(Double float64) {
      this.float64 = float64;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Float64)) {
          return false;
      }
      Float64 o = (Float64) other;
      return float64.equals(o.float64);
    }
    
    @Override
    public int hashCode() {
      return 2 * float64.hashCode();
    }
  }
}
