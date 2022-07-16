package hydra.core;

/**
 * The identifier of a function constructor
 */
public abstract class FunctionVariant {
  private FunctionVariant () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(CompareTo instance) ;
    
    R visit(Elimination instance) ;
    
    R visit(Lambda instance) ;
    
    R visit(Primitive instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(FunctionVariant instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(CompareTo instance) {
      return otherwise((instance));
    }
    
    default R visit(Elimination instance) {
      return otherwise((instance));
    }
    
    default R visit(Lambda instance) {
      return otherwise((instance));
    }
    
    default R visit(Primitive instance) {
      return otherwise((instance));
    }
  }
  
  public static final class CompareTo extends FunctionVariant {
    public CompareTo () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CompareTo)) {
        return false;
      }
      CompareTo o = (CompareTo) (other);
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
  
  public static final class Elimination extends FunctionVariant {
    public Elimination () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Elimination)) {
        return false;
      }
      Elimination o = (Elimination) (other);
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
  
  public static final class Lambda extends FunctionVariant {
    public Lambda () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lambda)) {
        return false;
      }
      Lambda o = (Lambda) (other);
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
  
  public static final class Primitive extends FunctionVariant {
    public Primitive () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primitive)) {
        return false;
      }
      Primitive o = (Primitive) (other);
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