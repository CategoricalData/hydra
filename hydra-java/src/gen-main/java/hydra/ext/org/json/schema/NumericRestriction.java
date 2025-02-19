// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.json.schema;

import java.io.Serializable;

public abstract class NumericRestriction implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.json.schema.NumericRestriction");
  
  public static final hydra.core.Name FIELD_NAME_MINIMUM = new hydra.core.Name("minimum");
  
  public static final hydra.core.Name FIELD_NAME_MAXIMUM = new hydra.core.Name("maximum");
  
  public static final hydra.core.Name FIELD_NAME_MULTIPLE_OF = new hydra.core.Name("multipleOf");
  
  private NumericRestriction () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Minimum instance) ;
    
    R visit(Maximum instance) ;
    
    R visit(MultipleOf instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NumericRestriction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Minimum instance) {
      return otherwise((instance));
    }
    
    default R visit(Maximum instance) {
      return otherwise((instance));
    }
    
    default R visit(MultipleOf instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Minimum extends hydra.ext.org.json.schema.NumericRestriction implements Serializable {
    public final hydra.ext.org.json.schema.Limit value;
    
    public Minimum (hydra.ext.org.json.schema.Limit value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Minimum)) {
        return false;
      }
      Minimum o = (Minimum) (other);
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
  
  public static final class Maximum extends hydra.ext.org.json.schema.NumericRestriction implements Serializable {
    public final hydra.ext.org.json.schema.Limit value;
    
    public Maximum (hydra.ext.org.json.schema.Limit value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Maximum)) {
        return false;
      }
      Maximum o = (Maximum) (other);
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
  
  public static final class MultipleOf extends hydra.ext.org.json.schema.NumericRestriction implements Serializable {
    public final Integer value;
    
    public MultipleOf (Integer value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MultipleOf)) {
        return false;
      }
      MultipleOf o = (MultipleOf) (other);
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