// Note: this is an automatically generated file. Do not edit.

package hydra.mantle;

import java.io.Serializable;

/**
 * An equality judgement: less than, equal to, or greater than
 */
public abstract class Comparison implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.mantle.Comparison");
  
  public static final hydra.core.Name FIELD_NAME_LESS_THAN = new hydra.core.Name("lessThan");
  
  public static final hydra.core.Name FIELD_NAME_EQUAL_TO = new hydra.core.Name("equalTo");
  
  public static final hydra.core.Name FIELD_NAME_GREATER_THAN = new hydra.core.Name("greaterThan");
  
  private Comparison () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(LessThan instance) ;
    
    R visit(EqualTo instance) ;
    
    R visit(GreaterThan instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Comparison instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(LessThan instance) {
      return otherwise((instance));
    }
    
    default R visit(EqualTo instance) {
      return otherwise((instance));
    }
    
    default R visit(GreaterThan instance) {
      return otherwise((instance));
    }
  }
  
  public static final class LessThan extends hydra.mantle.Comparison implements Serializable {
    public final Boolean value;
    
    public LessThan (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LessThan)) {
        return false;
      }
      LessThan o = (LessThan) (other);
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
  
  public static final class EqualTo extends hydra.mantle.Comparison implements Serializable {
    public final Boolean value;
    
    public EqualTo (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EqualTo)) {
        return false;
      }
      EqualTo o = (EqualTo) (other);
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
  
  public static final class GreaterThan extends hydra.mantle.Comparison implements Serializable {
    public final Boolean value;
    
    public GreaterThan (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GreaterThan)) {
        return false;
      }
      GreaterThan o = (GreaterThan) (other);
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
