package hydra.ext.coq.syntax;

public abstract class UniverseName {
  private UniverseName () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Qualid instance) ;
    
    R visit(Set instance) ;
    
    R visit(Prop instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UniverseName instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Qualid instance) {
      return otherwise((instance));
    }
    
    default R visit(Set instance) {
      return otherwise((instance));
    }
    
    default R visit(Prop instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Qualid extends UniverseName {
    public final Qualid value;
    
    public Qualid (Qualid value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Qualid)) {
        return false;
      }
      Qualid o = (Qualid) (other);
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
  
  public static final class Set extends UniverseName {
    public Set () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
        return false;
      }
      Set o = (Set) (other);
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
  
  public static final class Prop extends UniverseName {
    public Prop () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Prop)) {
        return false;
      }
      Prop o = (Prop) (other);
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