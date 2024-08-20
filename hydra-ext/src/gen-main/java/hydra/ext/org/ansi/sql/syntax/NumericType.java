// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public abstract class NumericType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/ansi/sql/syntax.NumericType");
  
  public static final hydra.core.Name FIELD_NAME_EXACT = new hydra.core.Name("exact");
  
  public static final hydra.core.Name FIELD_NAME_APPROXIMATE = new hydra.core.Name("approximate");
  
  private NumericType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Exact instance) ;
    
    R visit(Approximate instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NumericType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Exact instance) {
      return otherwise((instance));
    }
    
    default R visit(Approximate instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Exact extends hydra.ext.org.ansi.sql.syntax.NumericType implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.ExactNumericType value;
    
    public Exact (hydra.ext.org.ansi.sql.syntax.ExactNumericType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Exact)) {
        return false;
      }
      Exact o = (Exact) (other);
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
  
  public static final class Approximate extends hydra.ext.org.ansi.sql.syntax.NumericType implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.ApproximateNumericType value;
    
    public Approximate (hydra.ext.org.ansi.sql.syntax.ApproximateNumericType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Approximate)) {
        return false;
      }
      Approximate o = (Approximate) (other);
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