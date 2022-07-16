package hydra.ext.coq.syntax;

public abstract class GeneralizingBinder {
  private GeneralizingBinder () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Explicit instance) ;
    
    R visit(ImplicitMaximallyInserted instance) ;
    
    R visit(ImplicitNonMaximallyInserted instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GeneralizingBinder instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Explicit instance) {
      return otherwise((instance));
    }
    
    default R visit(ImplicitMaximallyInserted instance) {
      return otherwise((instance));
    }
    
    default R visit(ImplicitNonMaximallyInserted instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * Terms surrounded by `( ) introduce their free variables as explicit arguments
   */
  public static final class Explicit extends GeneralizingBinder {
    /**
     * Terms surrounded by `( ) introduce their free variables as explicit arguments
     */
    public final TypeclassConstraint value;
    
    public Explicit (TypeclassConstraint value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Explicit)) {
        return false;
      }
      Explicit o = (Explicit) (other);
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
   * Terms surrounded by `{ } introduce their free variables as maximally inserted implicit arguments
   */
  public static final class ImplicitMaximallyInserted extends GeneralizingBinder {
    /**
     * Terms surrounded by `{ } introduce their free variables as maximally inserted implicit arguments
     */
    public final TypeclassConstraint value;
    
    public ImplicitMaximallyInserted (TypeclassConstraint value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ImplicitMaximallyInserted)) {
        return false;
      }
      ImplicitMaximallyInserted o = (ImplicitMaximallyInserted) (other);
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
   * Terms surrounded by `[ ] introduce them as non-maximally inserted implicit arguments
   */
  public static final class ImplicitNonMaximallyInserted extends GeneralizingBinder {
    /**
     * Terms surrounded by `[ ] introduce them as non-maximally inserted implicit arguments
     */
    public final TypeclassConstraint value;
    
    public ImplicitNonMaximallyInserted (TypeclassConstraint value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ImplicitNonMaximallyInserted)) {
        return false;
      }
      ImplicitNonMaximallyInserted o = (ImplicitNonMaximallyInserted) (other);
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