package hydra.core;

/**
 * A function
 */
public abstract class Function<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Function");
  
  private Function () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(CompareTo instance) ;
    
    R visit(Elimination instance) ;
    
    R visit(Lambda instance) ;
    
    R visit(Primitive instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Function instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(CompareTo instance) {
      return otherwise((instance));
    }
    
    default R visit(Elimination instance) {
      return otherwise((instance));
    }
    
    default R visit(Lambda instance) {
      return otherwise((instance));
    }
    
    default R visit(Primitive instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * Compares a term with a given term of the same type, producing a Comparison
   */
  public static final class CompareTo<M> extends hydra.core.Function<M> {
    /**
     * Compares a term with a given term of the same type, producing a Comparison
     */
    public final hydra.core.Term<M> value;
    
    public CompareTo (hydra.core.Term<M> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CompareTo)) {
        return false;
      }
      CompareTo o = (CompareTo) (other);
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
   * An elimination for any of a few term variants
   */
  public static final class Elimination<M> extends hydra.core.Function<M> {
    /**
     * An elimination for any of a few term variants
     */
    public final hydra.core.Elimination<M> value;
    
    public Elimination (hydra.core.Elimination<M> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Elimination)) {
        return false;
      }
      Elimination o = (Elimination) (other);
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
   * A function abstraction (lambda)
   */
  public static final class Lambda<M> extends hydra.core.Function<M> {
    /**
     * A function abstraction (lambda)
     */
    public final hydra.core.Lambda<M> value;
    
    public Lambda (hydra.core.Lambda<M> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lambda)) {
        return false;
      }
      Lambda o = (Lambda) (other);
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
   * A reference to a built-in (primitive) function
   */
  public static final class Primitive<M> extends hydra.core.Function<M> {
    /**
     * A reference to a built-in (primitive) function
     */
    public final hydra.core.Name value;
    
    public Primitive (hydra.core.Name value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primitive)) {
        return false;
      }
      Primitive o = (Primitive) (other);
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