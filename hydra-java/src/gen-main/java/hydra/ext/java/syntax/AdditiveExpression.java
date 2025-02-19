// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class AdditiveExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.AdditiveExpression");
  
  public static final hydra.core.Name FIELD_NAME_UNARY = new hydra.core.Name("unary");
  
  public static final hydra.core.Name FIELD_NAME_PLUS = new hydra.core.Name("plus");
  
  public static final hydra.core.Name FIELD_NAME_MINUS = new hydra.core.Name("minus");
  
  private AdditiveExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Unary instance) ;
    
    R visit(Plus instance) ;
    
    R visit(Minus instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AdditiveExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Unary instance) {
      return otherwise((instance));
    }
    
    default R visit(Plus instance) {
      return otherwise((instance));
    }
    
    default R visit(Minus instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Unary extends hydra.ext.java.syntax.AdditiveExpression implements Serializable {
    public final hydra.ext.java.syntax.MultiplicativeExpression value;
    
    public Unary (hydra.ext.java.syntax.MultiplicativeExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unary)) {
        return false;
      }
      Unary o = (Unary) (other);
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
  
  public static final class Plus extends hydra.ext.java.syntax.AdditiveExpression implements Serializable {
    public final hydra.ext.java.syntax.AdditiveExpression_Binary value;
    
    public Plus (hydra.ext.java.syntax.AdditiveExpression_Binary value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Plus)) {
        return false;
      }
      Plus o = (Plus) (other);
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
  
  public static final class Minus extends hydra.ext.java.syntax.AdditiveExpression implements Serializable {
    public final hydra.ext.java.syntax.AdditiveExpression_Binary value;
    
    public Minus (hydra.ext.java.syntax.AdditiveExpression_Binary value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Minus)) {
        return false;
      }
      Minus o = (Minus) (other);
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