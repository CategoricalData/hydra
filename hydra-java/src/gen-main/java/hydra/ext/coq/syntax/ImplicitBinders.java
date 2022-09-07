package hydra.ext.coq.syntax;

/**
 * In the context of a function definition, these forms specify that name is an implicit argument.
 */
public abstract class ImplicitBinders {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.ImplicitBinders");
  
  private ImplicitBinders () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(MaximallyInserted instance) ;
    
    R visit(NonMaximallyInserted instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ImplicitBinders instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(MaximallyInserted instance) {
      return otherwise((instance));
    }
    
    default R visit(NonMaximallyInserted instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * The first form, with curly braces, makes name a maximally inserted implicit argument
   */
  public static final class MaximallyInserted extends hydra.ext.coq.syntax.ImplicitBinders {
    /**
     * The first form, with curly braces, makes name a maximally inserted implicit argument
     */
    public final hydra.ext.coq.syntax.TypeBinders value;
    
    public MaximallyInserted (hydra.ext.coq.syntax.TypeBinders value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MaximallyInserted)) {
        return false;
      }
      MaximallyInserted o = (MaximallyInserted) (other);
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
   * The second form, with square brackets, makes name a non-maximally inserted implicit argument.
   */
  public static final class NonMaximallyInserted extends hydra.ext.coq.syntax.ImplicitBinders {
    /**
     * The second form, with square brackets, makes name a non-maximally inserted implicit argument.
     */
    public final hydra.ext.coq.syntax.TypeBinders value;
    
    public NonMaximallyInserted (hydra.ext.coq.syntax.TypeBinders value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NonMaximallyInserted)) {
        return false;
      }
      NonMaximallyInserted o = (NonMaximallyInserted) (other);
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