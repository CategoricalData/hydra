package hydra.ext.sql.ansi;

public abstract class ApproximateNumericType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.ApproximateNumericType");
  
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
  
  public static final class Float_ extends hydra.ext.sql.ansi.ApproximateNumericType {
    public final java.util.Optional<hydra.ext.sql.ansi.Precision> value;
    
    public Float_ (java.util.Optional<hydra.ext.sql.ansi.Precision> value) {
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
  
  public static final class Real extends hydra.ext.sql.ansi.ApproximateNumericType {
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
  
  public static final class Double_ extends hydra.ext.sql.ansi.ApproximateNumericType {
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