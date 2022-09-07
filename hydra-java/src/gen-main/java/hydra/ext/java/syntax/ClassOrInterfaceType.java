package hydra.ext.java.syntax;

public abstract class ClassOrInterfaceType {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.ClassOrInterfaceType");
  
  private ClassOrInterfaceType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Class_ instance) ;
    
    R visit(Interface instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ClassOrInterfaceType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Class_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Interface instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Class_ extends hydra.ext.java.syntax.ClassOrInterfaceType {
    public final hydra.ext.java.syntax.ClassType value;
    
    public Class_ (hydra.ext.java.syntax.ClassType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Class_)) {
        return false;
      }
      Class_ o = (Class_) (other);
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
  
  public static final class Interface extends hydra.ext.java.syntax.ClassOrInterfaceType {
    public final hydra.ext.java.syntax.InterfaceType value;
    
    public Interface (hydra.ext.java.syntax.InterfaceType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Interface)) {
        return false;
      }
      Interface o = (Interface) (other);
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