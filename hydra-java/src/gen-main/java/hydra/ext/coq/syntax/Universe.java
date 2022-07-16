package hydra.ext.coq.syntax;

public abstract class Universe {
  private Universe () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Max instance) ;
    
    R visit(Expr instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Universe instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Max instance) {
      return otherwise((instance));
    }
    
    default R visit(Expr instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * Note: list cannot be empty
   */
  public static final class Max extends Universe {
    /**
     * Note: list cannot be empty
     */
    public final java.util.List<Universe_Expr> value;
    
    public Max (java.util.List<Universe_Expr> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Max)) {
        return false;
      }
      Max o = (Max) (other);
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
  
  public static final class Expr extends Universe {
    public final Universe_Expr value;
    
    public Expr (Universe_Expr value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Expr)) {
        return false;
      }
      Expr o = (Expr) (other);
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