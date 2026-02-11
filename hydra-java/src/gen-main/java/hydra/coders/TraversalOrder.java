// Note: this is an automatically generated file. Do not edit.

package hydra.coders;

import java.io.Serializable;

/**
 * Specifies either a pre-order or post-order traversal
 */
public abstract class TraversalOrder implements Serializable, Comparable<TraversalOrder> {
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Pre instance) {
      return otherwise(instance);
    }
    
    default R visit(Post instance) {
      return otherwise(instance);
    }
  }
  
  /**
   * Pre-order traversal
   */
  public static final class Pre extends hydra.coders.TraversalOrder implements Serializable {
    public Pre () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pre)) {
        return false;
      }
      Pre o = (Pre) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalOrder other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
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
  public static final class Post extends hydra.coders.TraversalOrder implements Serializable {
    public Post () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Post)) {
        return false;
      }
      Post o = (Post) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalOrder other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
