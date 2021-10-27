package hydra.core;

public abstract class Precision {
  private Precision() {}
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  /**
   * An interface for applying a function to a Precision according to its variant (subclass)
   */
  public interface Visitor<R> {
    R visit(Arbitrary instance) ;
    
    R visit(Bits instance) ;
  }
  
  /**
   * An interface for applying a function to a Precision according to its variant (subclass). If a visit() method for a
   * particular variant is not implemented, a default method is used instead.
   */
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Precision instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    @Override
    default R visit(Arbitrary instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Bits instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Arbitrary extends Precision {
    /**
     * Constructs an immutable Arbitrary object
     */
    public Arbitrary() {}
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Arbitrary)) {
          return false;
      }
      Arbitrary o = (Arbitrary) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
  }
  
  /**
   * @type integer
   */
  public static final class Bits extends Precision {
    public final Integer bits;
    
    /**
     * Constructs an immutable Bits object
     */
    public Bits(Integer bits) {
      this.bits = bits;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bits)) {
          return false;
      }
      Bits o = (Bits) other;
      return bits.equals(o.bits);
    }
    
    @Override
    public int hashCode() {
      return 2 * bits.hashCode();
    }
  }
}
