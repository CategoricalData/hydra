package hydra.ext.coq.syntax;

public abstract class Pattern10 {
  private Pattern10 () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(As instance) ;
    
    R visit(Patterns instance) ;
    
    R visit(Qualiid instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Pattern10 instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(As instance) {
      return otherwise((instance));
    }
    
    default R visit(Patterns instance) {
      return otherwise((instance));
    }
    
    default R visit(Qualiid instance) {
      return otherwise((instance));
    }
  }
  
  public static final class As extends Pattern10 {
    public final Pattern10_As value;
    
    public As (Pattern10_As value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof As)) {
        return false;
      }
      As o = (As) (other);
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
  
  public static final class Patterns extends Pattern10 {
    public final Pattern10_Patterns value;
    
    public Patterns (Pattern10_Patterns value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Patterns)) {
        return false;
      }
      Patterns o = (Patterns) (other);
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
  
  public static final class Qualiid extends Pattern10 {
    public final Pattern10_Qualid value;
    
    public Qualiid (Pattern10_Qualid value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Qualiid)) {
        return false;
      }
      Qualiid o = (Qualiid) (other);
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