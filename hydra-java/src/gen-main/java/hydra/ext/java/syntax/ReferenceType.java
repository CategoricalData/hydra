package hydra.ext.java.syntax;

public abstract class ReferenceType {
  private ReferenceType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(ClassOrInterface instance) ;
    
    R visit(Variable instance) ;
    
    R visit(Array instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ReferenceType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(ClassOrInterface instance) {
      return otherwise((instance));
    }
    
    default R visit(Variable instance) {
      return otherwise((instance));
    }
    
    default R visit(Array instance) {
      return otherwise((instance));
    }
  }
  
  public static final class ClassOrInterface extends ReferenceType {
    public final ClassOrInterfaceType value;
    
    public ClassOrInterface (ClassOrInterfaceType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClassOrInterface)) {
        return false;
      }
      ClassOrInterface o = (ClassOrInterface) (other);
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
  
  public static final class Variable extends ReferenceType {
    public final TypeVariable value;
    
    public Variable (TypeVariable value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variable)) {
        return false;
      }
      Variable o = (Variable) (other);
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
  
  public static final class Array extends ReferenceType {
    public final ArrayType value;
    
    public Array (ArrayType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Array)) {
        return false;
      }
      Array o = (Array) (other);
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