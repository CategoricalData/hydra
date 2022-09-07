package hydra.ext.java.syntax;

public abstract class IntegralType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.IntegralType");
  
  private IntegralType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Byte_ instance) ;
    
    R visit(Short_ instance) ;
    
    R visit(Int instance) ;
    
    R visit(Long_ instance) ;
    
    R visit(Char instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(IntegralType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Byte_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Short_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Int instance) {
      return otherwise((instance));
    }
    
    default R visit(Long_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Char instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Byte_ extends hydra.ext.java.syntax.IntegralType {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Byte");
    
    public Byte_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Byte_)) {
        return false;
      }
      Byte_ o = (Byte_) (other);
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
  
  public static final class Short_ extends hydra.ext.java.syntax.IntegralType {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Short");
    
    public Short_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Short_)) {
        return false;
      }
      Short_ o = (Short_) (other);
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
  
  public static final class Int extends hydra.ext.java.syntax.IntegralType {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Int");
    
    public Int () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int)) {
        return false;
      }
      Int o = (Int) (other);
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
  
  public static final class Long_ extends hydra.ext.java.syntax.IntegralType {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Long");
    
    public Long_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Long_)) {
        return false;
      }
      Long_ o = (Long_) (other);
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
  
  public static final class Char extends hydra.ext.java.syntax.IntegralType {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Char");
    
    public Char () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Char)) {
        return false;
      }
      Char o = (Char) (other);
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