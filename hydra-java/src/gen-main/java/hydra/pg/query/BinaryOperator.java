// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public abstract class BinaryOperator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/pg/query.BinaryOperator");
  
  public static final hydra.core.Name FIELD_NAME_BOOLEAN = new hydra.core.Name("boolean");
  
  public static final hydra.core.Name FIELD_NAME_COMPARISON = new hydra.core.Name("comparison");
  
  public static final hydra.core.Name FIELD_NAME_POWER = new hydra.core.Name("power");
  
  private BinaryOperator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Boolean_ instance) ;
    
    R visit(Comparison instance) ;
    
    R visit(Power instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BinaryOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Boolean_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Comparison instance) {
      return otherwise((instance));
    }
    
    default R visit(Power instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Boolean_ extends hydra.pg.query.BinaryOperator implements Serializable {
    public final hydra.pg.query.BinaryBooleanOperator value;
    
    public Boolean_ (hydra.pg.query.BinaryBooleanOperator value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Boolean_)) {
        return false;
      }
      Boolean_ o = (Boolean_) (other);
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
  
  public static final class Comparison extends hydra.pg.query.BinaryOperator implements Serializable {
    public final hydra.pg.query.ComparisonOperator value;
    
    public Comparison (hydra.pg.query.ComparisonOperator value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Comparison)) {
        return false;
      }
      Comparison o = (Comparison) (other);
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
  
  public static final class Power extends hydra.pg.query.BinaryOperator implements Serializable {
    public Power () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Power)) {
        return false;
      }
      Power o = (Power) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}