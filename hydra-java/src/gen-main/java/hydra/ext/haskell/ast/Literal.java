package hydra.ext.haskell.ast;

/**
 * A literal value
 */
public abstract class Literal {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.Literal");
  
  private Literal () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Char instance) ;
    
    R visit(Double_ instance) ;
    
    R visit(Float_ instance) ;
    
    R visit(Int instance) ;
    
    R visit(Integer_ instance) ;
    
    R visit(String_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Literal instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Char instance) {
      return otherwise((instance));
    }
    
    default R visit(Double_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Float_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Int instance) {
      return otherwise((instance));
    }
    
    default R visit(Integer_ instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Char extends hydra.ext.haskell.ast.Literal {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.Char");
    
    public final Character value;
    
    public Char (Character value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Char)) {
        return false;
      }
      Char o = (Char) (other);
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
  
  public static final class Double_ extends hydra.ext.haskell.ast.Literal {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.Double");
    
    public final Double value;
    
    public Double_ (Double value) {
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
  
  public static final class Float_ extends hydra.ext.haskell.ast.Literal {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.Float");
    
    public final Float value;
    
    public Float_ (Float value) {
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
  
  public static final class Int extends hydra.ext.haskell.ast.Literal {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.Int");
    
    public final Integer value;
    
    public Int (Integer value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Int)) {
        return false;
      }
      Int o = (Int) (other);
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
  
  public static final class Integer_ extends hydra.ext.haskell.ast.Literal {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.Integer");
    
    public final java.math.BigInteger value;
    
    public Integer_ (java.math.BigInteger value) {
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
  
  public static final class String_ extends hydra.ext.haskell.ast.Literal {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.String");
    
    public final String value;
    
    public String_ (String value) {
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