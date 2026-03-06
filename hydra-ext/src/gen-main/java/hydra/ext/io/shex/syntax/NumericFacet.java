// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class NumericFacet implements Serializable, Comparable<NumericFacet> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.NumericFacet");
  
  public static final hydra.core.Name SEQUENCE = new hydra.core.Name("sequence");
  
  public static final hydra.core.Name SEQUENCE2 = new hydra.core.Name("sequence2");
  
  private NumericFacet () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Sequence instance) ;
    
    R visit(Sequence2 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NumericFacet instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Sequence instance) {
      return otherwise(instance);
    }
    
    default R visit(Sequence2 instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Sequence extends hydra.ext.io.shex.syntax.NumericFacet implements Serializable {
    public final hydra.ext.io.shex.syntax.NumericFacet_Sequence value;
    
    public Sequence (hydra.ext.io.shex.syntax.NumericFacet_Sequence value) {
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
    public int compareTo(NumericFacet other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
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
  
  public static final class Sequence2 extends hydra.ext.io.shex.syntax.NumericFacet implements Serializable {
    public final hydra.ext.io.shex.syntax.NumericFacet_Sequence2 value;
    
    public Sequence2 (hydra.ext.io.shex.syntax.NumericFacet_Sequence2 value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence2)) {
        return false;
      }
      Sequence2 o = (Sequence2) other;
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
    public int compareTo(NumericFacet other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sequence2 o = (Sequence2) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
