// Note: this is an automatically generated file. Do not edit.

package hydra.ext.sql.ansi;

import java.io.Serializable;

public abstract class ApproximateNumericType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/sql/ansi.ApproximateNumericType");
  
  public static final hydra.core.Name FIELD_NAME_FLOAT = new hydra.core.Name("float");
  
  public static final hydra.core.Name FIELD_NAME_REAL = new hydra.core.Name("real");
  
  public static final hydra.core.Name FIELD_NAME_DOUBLE = new hydra.core.Name("double");
  
  private ApproximateNumericType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Float_ instance) ;
    
    R visit(Real instance) ;
    
    R visit(Double_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ApproximateNumericType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Float_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Real instance) {
      return otherwise((instance));
    }
    
    default R visit(Double_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Float_ extends hydra.ext.sql.ansi.ApproximateNumericType implements Serializable {
    public final hydra.util.Opt<hydra.ext.sql.ansi.Precision> value;
    
    public Float_ (hydra.util.Opt<hydra.ext.sql.ansi.Precision> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Float_)) {
        return false;
      }
      Float_ o = (Float_) (other);
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
  
  public static final class Real extends hydra.ext.sql.ansi.ApproximateNumericType implements Serializable {
    public Real () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Real)) {
        return false;
      }
      Real o = (Real) (other);
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
  
  public static final class Double_ extends hydra.ext.sql.ansi.ApproximateNumericType implements Serializable {
    public Double_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Double_)) {
        return false;
      }
      Double_ o = (Double_) (other);
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
