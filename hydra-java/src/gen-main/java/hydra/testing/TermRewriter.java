// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A predefined term rewriter for testing rewriteTerm
 */
public abstract class TermRewriter implements Serializable, Comparable<TermRewriter> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.TermRewriter");
  
  public static final hydra.core.Name FIELD_NAME_REPLACE_FOO_WITH_BAR = new hydra.core.Name("replaceFooWithBar");
  
  public static final hydra.core.Name FIELD_NAME_REPLACE_INT32_WITH_INT64 = new hydra.core.Name("replaceInt32WithInt64");
  
  private TermRewriter () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(ReplaceFooWithBar instance) ;
    
    R visit(ReplaceInt32WithInt64 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TermRewriter instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(ReplaceFooWithBar instance) {
      return otherwise(instance);
    }
    
    default R visit(ReplaceInt32WithInt64 instance) {
      return otherwise(instance);
    }
  }
  
  /**
   * Replace all string literal 'foo' with 'bar'
   */
  public static final class ReplaceFooWithBar extends hydra.testing.TermRewriter implements Serializable {
    public ReplaceFooWithBar () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ReplaceFooWithBar)) {
        return false;
      }
      ReplaceFooWithBar o = (ReplaceFooWithBar) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TermRewriter other) {
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
   * Replace all Int32 literals with Int64 literals of the same value
   */
  public static final class ReplaceInt32WithInt64 extends hydra.testing.TermRewriter implements Serializable {
    public ReplaceInt32WithInt64 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ReplaceInt32WithInt64)) {
        return false;
      }
      ReplaceInt32WithInt64 o = (ReplaceInt32WithInt64) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TermRewriter other) {
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
