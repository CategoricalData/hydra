package hydra.compute;

/**
 * Specifies either a pre-order or post-order traversal
 */
public abstract class TraversalOrder {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/compute.TraversalOrder");
  
  private TraversalOrder () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Pre instance) ;
    
    R visit(Post instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TraversalOrder instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Pre instance) {
      return otherwise((instance));
    }
    
    default R visit(Post instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * Pre-order traversal
   */
  public static final class Pre extends hydra.compute.TraversalOrder {
    public Pre () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pre)) {
        return false;
      }
      Pre o = (Pre) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * Post-order traversal
   */
  public static final class Post extends hydra.compute.TraversalOrder {
    public Post () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Post)) {
        return false;
      }
      Post o = (Post) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}