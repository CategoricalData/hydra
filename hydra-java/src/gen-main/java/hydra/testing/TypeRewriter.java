// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A predefined type rewriter for testing rewriteType
 */
public abstract class TypeRewriter implements Serializable, Comparable<TypeRewriter> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.TypeRewriter");
  
  public static final hydra.core.Name FIELD_NAME_REPLACE_STRING_WITH_INT32 = new hydra.core.Name("replaceStringWithInt32");
  
  private TypeRewriter () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(ReplaceStringWithInt32 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TypeRewriter instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(ReplaceStringWithInt32 instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * Replace all String types with Int32 types
   */
  public static final class ReplaceStringWithInt32 extends hydra.testing.TypeRewriter implements Serializable {
    public ReplaceStringWithInt32 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ReplaceStringWithInt32)) {
        return false;
      }
      ReplaceStringWithInt32 o = (ReplaceStringWithInt32) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeRewriter other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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
