// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class ClassBase implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.ClassBase");
  
  public static final hydra.core.Name FIELD_NAME_CLASS = new hydra.core.Name("class");
  
  public static final hydra.core.Name FIELD_NAME_INTERFACES = new hydra.core.Name("interfaces");
  
  private ClassBase () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Class_ instance) ;
    
    R visit(Interfaces instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ClassBase instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Class_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Interfaces instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Class_ extends hydra.ext.csharp.syntax.ClassBase implements Serializable {
    public final hydra.util.Opt<hydra.ext.csharp.syntax.ClassType> value;
    
    public Class_ (hydra.util.Opt<hydra.ext.csharp.syntax.ClassType> value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Interfaces extends hydra.ext.csharp.syntax.ClassBase implements Serializable {
    public final java.util.List<hydra.ext.csharp.syntax.InterfaceType> value;
    
    public Interfaces (java.util.List<hydra.ext.csharp.syntax.InterfaceType> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Interfaces)) {
        return false;
      }
      Interfaces o = (Interfaces) (other);
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