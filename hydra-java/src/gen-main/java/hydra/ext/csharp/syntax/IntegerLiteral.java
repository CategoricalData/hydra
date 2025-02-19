// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class IntegerLiteral implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.IntegerLiteral");
  
  public static final hydra.core.Name FIELD_NAME_DECIMAL = new hydra.core.Name("decimal");
  
  public static final hydra.core.Name FIELD_NAME_HEXADECIMAL = new hydra.core.Name("hexadecimal");
  
  public static final hydra.core.Name FIELD_NAME_BINARY = new hydra.core.Name("binary");
  
  private IntegerLiteral () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Decimal instance) ;
    
    R visit(Hexadecimal instance) ;
    
    R visit(Binary instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(IntegerLiteral instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Decimal instance) {
      return otherwise((instance));
    }
    
    default R visit(Hexadecimal instance) {
      return otherwise((instance));
    }
    
    default R visit(Binary instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Decimal extends hydra.ext.csharp.syntax.IntegerLiteral implements Serializable {
    public final String value;
    
    public Decimal (String value) {
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
  
  public static final class Hexadecimal extends hydra.ext.csharp.syntax.IntegerLiteral implements Serializable {
    public final String value;
    
    public Hexadecimal (String value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Hexadecimal)) {
        return false;
      }
      Hexadecimal o = (Hexadecimal) (other);
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
  
  public static final class Binary extends hydra.ext.csharp.syntax.IntegerLiteral implements Serializable {
    public final java.math.BigInteger value;
    
    public Binary (java.math.BigInteger value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Binary)) {
        return false;
      }
      Binary o = (Binary) (other);
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