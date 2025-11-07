// Note: this is an automatically generated file. Do not edit.

package hydra.coders;

import java.io.Serializable;

/**
 * Specifies either a pre-order or post-order traversal
 */
public abstract class TraversalOrder implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.coders.TraversalOrder");
  
  public static final hydra.core.Name FIELD_NAME_PRE = new hydra.core.Name("pre");
  
  public static final hydra.core.Name FIELD_NAME_POST = new hydra.core.Name("post");
  
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
  public static final class Pre extends hydra.coders.TraversalOrder implements Serializable {
    public final Boolean value;
    
    public Pre (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pre)) {
        return false;
      }
      Pre o = (Pre) (other);
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
   * Post-order traversal
   */
  public static final class Post extends hydra.coders.TraversalOrder implements Serializable {
    public final Boolean value;
    
    public Post (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Post)) {
        return false;
      }
      Post o = (Post) (other);
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
