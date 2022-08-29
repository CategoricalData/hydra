package hydra.ext.java.syntax;

public abstract class WildcardBounds {
  private WildcardBounds () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Extends instance) ;
    
    R visit(Super instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(WildcardBounds instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Extends instance) {
      return otherwise((instance));
    }
    
    default R visit(Super instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Extends extends hydra.ext.java.syntax.WildcardBounds {
    public final hydra.ext.java.syntax.ReferenceType value;
    
    public Extends (hydra.ext.java.syntax.ReferenceType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Extends)) {
        return false;
      }
      Extends o = (Extends) (other);
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
  
  public static final class Super extends hydra.ext.java.syntax.WildcardBounds {
    public final hydra.ext.java.syntax.ReferenceType value;
    
    public Super (hydra.ext.java.syntax.ReferenceType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Super)) {
        return false;
      }
      Super o = (Super) (other);
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