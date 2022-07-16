package hydra.ext.java.syntax;

public abstract class ClassTypeQualifier {
  private ClassTypeQualifier () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(None instance) ;
    
    R visit(Package_ instance) ;
    
    R visit(Parent instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ClassTypeQualifier instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(None instance) {
      return otherwise((instance));
    }
    
    default R visit(Package_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Parent instance) {
      return otherwise((instance));
    }
  }
  
  public static final class None extends ClassTypeQualifier {
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
  
  public static final class Package_ extends ClassTypeQualifier {
    public final PackageName value;
    
    public Package_ (PackageName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Package_)) {
        return false;
      }
      Package_ o = (Package_) (other);
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
  
  public static final class Parent extends ClassTypeQualifier {
    public final ClassOrInterfaceType value;
    
    public Parent (ClassOrInterfaceType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parent)) {
        return false;
      }
      Parent o = (Parent) (other);
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