// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A floating-point type
 */
public abstract class FloatType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.FloatType");
  
  public static final hydra.core.Name FIELD_NAME_BIGFLOAT = new hydra.core.Name("bigfloat");
  
  public static final hydra.core.Name FIELD_NAME_FLOAT32 = new hydra.core.Name("float32");
  
  public static final hydra.core.Name FIELD_NAME_FLOAT64 = new hydra.core.Name("float64");
  
  private FloatType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Bigfloat instance) ;
    
    R visit(Float32 instance) ;
    
    R visit(Float64 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(FloatType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Bigfloat instance) {
      return otherwise((instance));
    }
    
    default R visit(Float32 instance) {
      return otherwise((instance));
    }
    
    default R visit(Float64 instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Bigfloat extends hydra.core.FloatType implements Serializable {
    public Bigfloat () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bigfloat)) {
        return false;
      }
      Bigfloat o = (Bigfloat) (other);
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
  
  public static final class Float32 extends hydra.core.FloatType implements Serializable {
    public Float32 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Float32)) {
        return false;
      }
      Float32 o = (Float32) (other);
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
  
  public static final class Float64 extends hydra.core.FloatType implements Serializable {
    public Float64 () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Float64)) {
        return false;
      }
      Float64 o = (Float64) (other);
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