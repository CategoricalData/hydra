// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class Literal implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.Literal");
  
  public static final hydra.core.Name FIELD_NAME_BOOLEAN = new hydra.core.Name("boolean");
  
  public static final hydra.core.Name FIELD_NAME_INTEGER = new hydra.core.Name("integer");
  
  public static final hydra.core.Name FIELD_NAME_REAL = new hydra.core.Name("real");
  
  public static final hydra.core.Name FIELD_NAME_CHARACTER = new hydra.core.Name("character");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name FIELD_NAME_NULL = new hydra.core.Name("null");
  
  private Literal () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Boolean_ instance) ;
    
    R visit(Integer_ instance) ;
    
    R visit(Real instance) ;
    
    R visit(Character_ instance) ;
    
    R visit(String_ instance) ;
    
    R visit(Null instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Literal instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Boolean_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Integer_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Real instance) {
      return otherwise((instance));
    }
    
    default R visit(Character_ instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Null instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Boolean_ extends hydra.ext.csharp.syntax.Literal implements Serializable {
    public final Boolean value;
    
    public Boolean_ (Boolean value) {
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
  
  public static final class Integer_ extends hydra.ext.csharp.syntax.Literal implements Serializable {
    public final hydra.ext.csharp.syntax.IntegerLiteral value;
    
    public Integer_ (hydra.ext.csharp.syntax.IntegerLiteral value) {
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
  
  public static final class Real extends hydra.ext.csharp.syntax.Literal implements Serializable {
    public final Double value;
    
    public Real (Double value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Real)) {
        return false;
      }
      Real o = (Real) (other);
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
  
  public static final class Character_ extends hydra.ext.csharp.syntax.Literal implements Serializable {
    public final String value;
    
    public Character_ (String value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Character_)) {
        return false;
      }
      Character_ o = (Character_) (other);
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
  
  public static final class String_ extends hydra.ext.csharp.syntax.Literal implements Serializable {
    public final String value;
    
    public String_ (String value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) (other);
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
  
  public static final class Null extends hydra.ext.csharp.syntax.Literal implements Serializable {
    public Null () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Null)) {
        return false;
      }
      Null o = (Null) (other);
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