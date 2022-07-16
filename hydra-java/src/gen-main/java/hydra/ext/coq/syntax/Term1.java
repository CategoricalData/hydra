package hydra.ext.coq.syntax;

public abstract class Term1 {
  private Term1 () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Projection instance) ;
    
    R visit(Scope instance) ;
    
    R visit(Term0 instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Term1 instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Projection instance) {
      return otherwise((instance));
    }
    
    default R visit(Scope instance) {
      return otherwise((instance));
    }
    
    default R visit(Term0 instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Projection extends Term1 {
    public Projection () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Projection)) {
        return false;
      }
      Projection o = (Projection) (other);
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
  
  public static final class Scope extends Term1 {
    public Scope () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Scope)) {
        return false;
      }
      Scope o = (Scope) (other);
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
  
  public static final class Term0 extends Term1 {
    public final Term0 value;
    
    public Term0 (Term0 value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Term0)) {
        return false;
      }
      Term0 o = (Term0) (other);
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