package hydra.ext.java.syntax;

public abstract class ClassMemberDeclaration {
  private ClassMemberDeclaration () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Field instance) ;
    
    R visit(Method instance) ;
    
    R visit(Class_ instance) ;
    
    R visit(Interface instance) ;
    
    R visit(None instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ClassMemberDeclaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Field instance) {
      return otherwise((instance));
    }
    
    default R visit(Method instance) {
      return otherwise((instance));
    }
    
    default R visit(Class_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Interface instance) {
      return otherwise((instance));
    }
    
    default R visit(None instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Field extends hydra.ext.java.syntax.ClassMemberDeclaration {
    public final hydra.ext.java.syntax.FieldDeclaration value;
    
    public Field (hydra.ext.java.syntax.FieldDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Field)) {
        return false;
      }
      Field o = (Field) (other);
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
  
  public static final class Method extends hydra.ext.java.syntax.ClassMemberDeclaration {
    public final hydra.ext.java.syntax.MethodDeclaration value;
    
    public Method (hydra.ext.java.syntax.MethodDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Method)) {
        return false;
      }
      Method o = (Method) (other);
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
  
  public static final class Class_ extends hydra.ext.java.syntax.ClassMemberDeclaration {
    public final hydra.ext.java.syntax.ClassDeclaration value;
    
    public Class_ (hydra.ext.java.syntax.ClassDeclaration value) {
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
  
  public static final class Interface extends hydra.ext.java.syntax.ClassMemberDeclaration {
    public final hydra.ext.java.syntax.InterfaceDeclaration value;
    
    public Interface (hydra.ext.java.syntax.InterfaceDeclaration value) {
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
  
  public static final class None extends hydra.ext.java.syntax.ClassMemberDeclaration {
    public None () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof None)) {
        return false;
      }
      None o = (None) (other);
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