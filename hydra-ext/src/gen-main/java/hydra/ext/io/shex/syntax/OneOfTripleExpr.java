// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class OneOfTripleExpr implements Serializable, Comparable<OneOfTripleExpr> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.OneOfTripleExpr");
  
  public static final hydra.core.Name GROUP_TRIPLE_EXPR = new hydra.core.Name("GroupTripleExpr");
  
  public static final hydra.core.Name MULTI_ELEMENT_ONE_OF = new hydra.core.Name("MultiElementOneOf");
  
  private OneOfTripleExpr () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(GroupTripleExpr instance) ;
    
    R visit(MultiElementOneOf instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OneOfTripleExpr instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(GroupTripleExpr instance) {
      return otherwise(instance);
    }
    
    default R visit(MultiElementOneOf instance) {
      return otherwise(instance);
    }
  }
  
  public static final class GroupTripleExpr extends hydra.ext.io.shex.syntax.OneOfTripleExpr implements Serializable {
    public final hydra.ext.io.shex.syntax.GroupTripleExpr value;
    
    public GroupTripleExpr (hydra.ext.io.shex.syntax.GroupTripleExpr value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GroupTripleExpr)) {
        return false;
      }
      GroupTripleExpr o = (GroupTripleExpr) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(OneOfTripleExpr other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      GroupTripleExpr o = (GroupTripleExpr) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class MultiElementOneOf extends hydra.ext.io.shex.syntax.OneOfTripleExpr implements Serializable {
    public final hydra.ext.io.shex.syntax.MultiElementOneOf value;
    
    public MultiElementOneOf (hydra.ext.io.shex.syntax.MultiElementOneOf value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MultiElementOneOf)) {
        return false;
      }
      MultiElementOneOf o = (MultiElementOneOf) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(OneOfTripleExpr other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MultiElementOneOf o = (MultiElementOneOf) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
