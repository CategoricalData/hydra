package hydra.ext.java.syntax;

public abstract class Literal {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Literal");
  
  private Literal () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Null instance) ;
    
    R visit(Integer_ instance) ;
    
    R visit(FloatingPoint instance) ;
    
    R visit(Boolean_ instance) ;
    
    R visit(Character_ instance) ;
    
    R visit(String_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Literal instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Null instance) {
      return otherwise((instance));
    }
    
    default R visit(Integer_ instance) {
      return otherwise((instance));
    }
    
    default R visit(FloatingPoint instance) {
      return otherwise((instance));
    }
    
    default R visit(Boolean_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Character_ instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Null extends hydra.ext.java.syntax.Literal {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Null");
    
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
  
  public static final class Integer_ extends hydra.ext.java.syntax.Literal {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Integer");
    
    public final hydra.ext.java.syntax.IntegerLiteral value;
    
    public Integer_ (hydra.ext.java.syntax.IntegerLiteral value) {
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
  
  public static final class FloatingPoint extends hydra.ext.java.syntax.Literal {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.FloatingPoint");
    
    public final hydra.ext.java.syntax.FloatingPointLiteral value;
    
    public FloatingPoint (hydra.ext.java.syntax.FloatingPointLiteral value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FloatingPoint)) {
        return false;
      }
      FloatingPoint o = (FloatingPoint) (other);
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
  
  public static final class Boolean_ extends hydra.ext.java.syntax.Literal {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Boolean");
    
    public final Boolean value;
    
    public Boolean_ (Boolean value) {
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
  
  public static final class Character_ extends hydra.ext.java.syntax.Literal {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Character");
    
    public final Character value;
    
    public Character_ (Character value) {
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
  
  public static final class String_ extends hydra.ext.java.syntax.Literal {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.String");
    
    public final hydra.ext.java.syntax.StringLiteral value;
    
    public String_ (hydra.ext.java.syntax.StringLiteral value) {
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
}