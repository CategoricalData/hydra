// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class UnaryTripleExpr implements Serializable, Comparable<UnaryTripleExpr> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.UnaryTripleExpr");
  
  public static final hydra.core.Name SEQUENCE = new hydra.core.Name("sequence");
  
  public static final hydra.core.Name INCLUDE = new hydra.core.Name("Include");
  
  private UnaryTripleExpr () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Sequence instance) ;
    
    R visit(Include instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UnaryTripleExpr instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Sequence instance) {
      return otherwise(instance);
    }
    
    default R visit(Include instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Sequence extends hydra.ext.io.shex.syntax.UnaryTripleExpr implements Serializable {
    public final hydra.ext.io.shex.syntax.UnaryTripleExpr_Sequence value;
    
    public Sequence (hydra.ext.io.shex.syntax.UnaryTripleExpr_Sequence value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence)) {
        return false;
      }
      Sequence o = (Sequence) other;
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
    public int compareTo(UnaryTripleExpr other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sequence o = (Sequence) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Include extends hydra.ext.io.shex.syntax.UnaryTripleExpr implements Serializable {
    public final hydra.ext.io.shex.syntax.Include value;
    
    public Include (hydra.ext.io.shex.syntax.Include value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Include)) {
        return false;
      }
      Include o = (Include) other;
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
    public int compareTo(UnaryTripleExpr other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Include o = (Include) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
