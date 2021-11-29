package hydra.core;

/**
 * An encoded optional value, for languages which do not natively support optionals
 */
public abstract class OptionalExpression<A> {
  private OptionalExpression() {}
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  /**
   * An interface for applying a function to a OptionalExpression according to its variant (subclass)
   */
  public interface Visitor<R> {
    R visit(Just instance) ;
    
    R visit(Nothing instance) ;
  }
  
  /**
   * An interface for applying a function to a OptionalExpression according to its variant (subclass). If a visit() method
   * for a particular variant is not implemented, a default method is used instead.
   */
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OptionalExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    @Override
    default R visit(Just instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Nothing instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Just<A> extends OptionalExpression<A> {
    public final hydra.core.Term<A> just;
    
    /**
     * Constructs an immutable Just object
     */
    public Just(hydra.core.Term<A> just) {
      this.just = just;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Just)) {
          return false;
      }
      Just o = (Just) other;
      return just.equals(o.just);
    }
    
    @Override
    public int hashCode() {
      return 2 * just.hashCode();
    }
  }
  
  public static final class Nothing<A> extends OptionalExpression<A> {
    /**
     * Constructs an immutable Nothing object
     */
    public Nothing() {}
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nothing)) {
          return false;
      }
      Nothing o = (Nothing) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
  }
}
