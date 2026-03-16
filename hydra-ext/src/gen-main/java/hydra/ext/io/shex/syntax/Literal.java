// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class Literal implements Serializable, Comparable<Literal> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.Literal");
  
  public static final hydra.core.Name RDF_LITERAL = new hydra.core.Name("RdfLiteral");
  
  public static final hydra.core.Name NUMERIC_LITERAL = new hydra.core.Name("NumericLiteral");
  
  public static final hydra.core.Name BOOLEAN_LITERAL = new hydra.core.Name("BooleanLiteral");
  
  private Literal () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(RdfLiteral instance) ;
    
    R visit(NumericLiteral instance) ;
    
    R visit(BooleanLiteral instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Literal instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(RdfLiteral instance) {
      return otherwise(instance);
    }
    
    default R visit(NumericLiteral instance) {
      return otherwise(instance);
    }
    
    default R visit(BooleanLiteral instance) {
      return otherwise(instance);
    }
  }
  
  public static final class RdfLiteral extends hydra.ext.io.shex.syntax.Literal implements Serializable {
    public final hydra.ext.io.shex.syntax.RdfLiteral value;
    
    public RdfLiteral (hydra.ext.io.shex.syntax.RdfLiteral value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RdfLiteral)) {
        return false;
      }
      RdfLiteral o = (RdfLiteral) other;
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
    public int compareTo(Literal other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      RdfLiteral o = (RdfLiteral) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class NumericLiteral extends hydra.ext.io.shex.syntax.Literal implements Serializable {
    public final hydra.ext.io.shex.syntax.NumericLiteral value;
    
    public NumericLiteral (hydra.ext.io.shex.syntax.NumericLiteral value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NumericLiteral)) {
        return false;
      }
      NumericLiteral o = (NumericLiteral) other;
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
    public int compareTo(Literal other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NumericLiteral o = (NumericLiteral) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class BooleanLiteral extends hydra.ext.io.shex.syntax.Literal implements Serializable {
    public final hydra.ext.io.shex.syntax.BooleanLiteral value;
    
    public BooleanLiteral (hydra.ext.io.shex.syntax.BooleanLiteral value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BooleanLiteral)) {
        return false;
      }
      BooleanLiteral o = (BooleanLiteral) other;
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
    public int compareTo(Literal other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      BooleanLiteral o = (BooleanLiteral) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
