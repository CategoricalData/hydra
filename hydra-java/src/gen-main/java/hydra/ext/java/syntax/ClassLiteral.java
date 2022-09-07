package hydra.ext.java.syntax;

public abstract class ClassLiteral {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.ClassLiteral");
  
  private ClassLiteral () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Type instance) ;
    
    R visit(NumericType instance) ;
    
    R visit(Boolean_ instance) ;
    
    R visit(Void_ instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ClassLiteral instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Type instance) {
      return otherwise((instance));
    }
    
    default R visit(NumericType instance) {
      return otherwise((instance));
    }
    
    default R visit(Boolean_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Void_ instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Type extends hydra.ext.java.syntax.ClassLiteral {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Type");
    
    public final hydra.ext.java.syntax.TypeNameArray value;
    
    public Type (hydra.ext.java.syntax.TypeNameArray value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Type)) {
        return false;
      }
      Type o = (Type) (other);
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
  
  public static final class NumericType extends hydra.ext.java.syntax.ClassLiteral {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.NumericType");
    
    public final hydra.ext.java.syntax.NumericTypeArray value;
    
    public NumericType (hydra.ext.java.syntax.NumericTypeArray value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NumericType)) {
        return false;
      }
      NumericType o = (NumericType) (other);
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
  
  public static final class Boolean_ extends hydra.ext.java.syntax.ClassLiteral {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Boolean");
    
    public final hydra.ext.java.syntax.BooleanArray value;
    
    public Boolean_ (hydra.ext.java.syntax.BooleanArray value) {
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
  
  public static final class Void_ extends hydra.ext.java.syntax.ClassLiteral {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Void");
    
    public Void_ () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Void_)) {
        return false;
      }
      Void_ o = (Void_) (other);
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