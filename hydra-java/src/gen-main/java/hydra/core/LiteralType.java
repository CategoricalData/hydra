package hydra.core;

/**
 * Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants
 */
public abstract class LiteralType {
  private LiteralType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Binary instance) ;
    
    R visit(Boolean_ instance) ;
    
    R visit(Float_ instance) ;
    
    R visit(Integer_ instance) ;
    
    R visit(String_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LiteralType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Binary instance) {
      return otherwise((instance));
    }
    
    default R visit(Boolean_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Float_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Integer_ instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Binary extends LiteralType {
    public Binary () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Binary)) {
        return false;
      }
      Binary o = (Binary) (other);
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
  
  public static final class Boolean_ extends LiteralType {
    public Boolean_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Boolean_)) {
        return false;
      }
      Boolean_ o = (Boolean_) (other);
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
  
  public static final class Float_ extends LiteralType {
    public final FloatType value;
    
    public Float_ (FloatType value) {
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
  
  public static final class Integer_ extends LiteralType {
    public final IntegerType value;
    
    public Integer_ (IntegerType value) {
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
  
  public static final class String_ extends LiteralType {
    public String_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) (other);
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