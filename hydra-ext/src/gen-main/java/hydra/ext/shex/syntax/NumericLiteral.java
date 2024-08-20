// Note: this is an automatically generated file. Do not edit.

package hydra.ext.shex.syntax;

import java.io.Serializable;

public abstract class NumericLiteral implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/shex/syntax.NumericLiteral");
  
  public static final hydra.core.Name FIELD_NAME_INTEGER = new hydra.core.Name("integer");
  
  public static final hydra.core.Name FIELD_NAME_DECIMAL = new hydra.core.Name("decimal");
  
  public static final hydra.core.Name FIELD_NAME_DOUBLE = new hydra.core.Name("double");
  
  private NumericLiteral () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Integer_ instance) ;
    
    R visit(Decimal instance) ;
    
    R visit(Double_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NumericLiteral instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Integer_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Decimal instance) {
      return otherwise((instance));
    }
    
    default R visit(Double_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Integer_ extends hydra.ext.shex.syntax.NumericLiteral implements Serializable {
    public final hydra.ext.shex.syntax.Integer_ value;
    
    public Integer_ (hydra.ext.shex.syntax.Integer_ value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Integer_)) {
        return false;
      }
      Integer_ o = (Integer_) (other);
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
  
  public static final class Decimal extends hydra.ext.shex.syntax.NumericLiteral implements Serializable {
    public final hydra.ext.shex.syntax.Decimal value;
    
    public Decimal (hydra.ext.shex.syntax.Decimal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Decimal)) {
        return false;
      }
      Decimal o = (Decimal) (other);
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
  
  public static final class Double_ extends hydra.ext.shex.syntax.NumericLiteral implements Serializable {
    public final hydra.ext.shex.syntax.Double_ value;
    
    public Double_ (hydra.ext.shex.syntax.Double_ value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Double_)) {
        return false;
      }
      Double_ o = (Double_) (other);
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
